/* omxtxres.c
 *
 * (c) 2012 Dickon Hood <dickon@fluff.org>
 * modified dec 2012 by Arjen V
 * Added resizer functionality
 * Added pixelaspectratio (hardware)
 *
 * A trivial OpenMAX transcoder for the Pi.
 *
 * Very much a work-in-progress, and as such is noisy, doesn't produce
 * particularly pretty output, and is probably buggier than a swamp in
 * summer.  Beware of memory leaks.
 *
 * Usage: ./omxtx [-v] [-b bitrate] [-s WxH] input.foo output.m4v
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

/* To do:
 *
 *  *  Flush the buffers at the end
 *  *  Sort out the PTSes
 *  *  Read up on buffer timings in general
 *  *  Feed the packets to AVFormat rather than dumping them raw to disc
 *  *  Interleave correctly with the other AV packets in the stream, rather
 *     than just dropping them entirely
 */

#define _BSD_SOURCE
#define FF_API_CODEC_ID

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "bcm_host.h"
#include "ilclient.h"
#include "libavformat/avformat.h"
#include "libavutil/avutil.h"
#include "libavcodec/avcodec.h"
#include <error.h>

#include "OMX_Video.h"
#include "OMX_Types.h"
#include "OMX_Component.h"
#include "OMX_Core.h"
#include "OMX_Broadcom.h"

#include <pthread.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <time.h>
#include <errno.h>

#include <unistd.h>

#define SPECVERSIONMAJOR	(1)
#define SPECVERSIONMINOR	(1)
#define	SPECREVISION		(2)
#define SPECSTEP		(0)

static void print_codecs(void)
{
	printf("Codecs\n");
    	printf("OMX_VIDEO_CodingUnused\t%d\n", OMX_VIDEO_CodingUnused);
    	printf("OMX_VIDEO_CodingMPEG2\t%d\n", OMX_VIDEO_CodingMPEG2);
    	printf("OMX_VIDEO_CodingH263\t%d\n", OMX_VIDEO_CodingH263);
    	printf("OMX_VIDEO_CodingMPEG4\t%d\n", OMX_VIDEO_CodingMPEG4);
    	printf("OMX_VIDEO_CodingWMV\t%d\n", OMX_VIDEO_CodingWMV);
    	printf("OMX_VIDEO_CodingRV\t%d\n", OMX_VIDEO_CodingRV);
    	printf("OMX_VIDEO_CodingAVC\t%d\n", OMX_VIDEO_CodingAVC);
    	printf("OMX_VIDEO_CodingMJPEG\t%d\n", OMX_VIDEO_CodingMJPEG);
    	printf("OMX_VIDEO_CodingKhronosExtensions\t%d\n", OMX_VIDEO_CodingKhronosExtensions);
    	printf("OMX_VIDEO_CodingVendorStartUnused\t%d\n", OMX_VIDEO_CodingVendorStartUnused);
    	printf("OMX_VIDEO_CodingVP6\t%d\n", OMX_VIDEO_CodingVP6);
    	printf("OMX_VIDEO_CodingVP7\t%d\n", OMX_VIDEO_CodingVP7);
    	printf("OMX_VIDEO_CodingVP8\t%d\n", OMX_VIDEO_CodingVP8);
    	printf("OMX_VIDEO_CodingYUV\t%d\n", OMX_VIDEO_CodingYUV);
    	printf("OMX_VIDEO_CodingSorenson\t%d\n", OMX_VIDEO_CodingSorenson);
    	printf("OMX_VIDEO_CodingTheora\t%d\n", OMX_VIDEO_CodingTheora);
    	printf("OMX_VIDEO_CodingMVC\t%d\n", OMX_VIDEO_CodingMVC);
}

/* Hateful things: */
#define MAKEMEvar(y, x, l) do {	OMX_VERSIONTYPE *v;			\
				y = calloc(sizeof(x) + l, 1);		\
				y->nSize = sizeof(x) + l;		\
				v = (void *) &(((OMX_U32 *)y)[1]);	\
				v->s.nVersionMajor = SPECVERSIONMAJOR;	\
				v->s.nVersionMinor = SPECVERSIONMINOR;	\
				v->s.nRevision = SPECREVISION;		\
				v->s.nStep = SPECSTEP;			\
				} while (0) /* Yes, the void * is evil */

#define MAKEME(y, x)	 	MAKEMEvar(y, x, 0)

#define OERR(cmd, verbose)	do {					\
				OMX_ERRORTYPE oerr = cmd;		\
				if (oerr != OMX_ErrorNone) {		\
					fprintf(stderr, #cmd		\
						" failed on line %d: %x\n", \
						__LINE__, oerr);	\
					exit(1);			\
				} else {				\
					if (verbose)			\
					fprintf(stderr, #cmd		\
						" completed OK.\n");	\
				}					\
			} while (0)

#define OERRq(cmd)	do {						\
				OMX_ERRORTYPE oerr = cmd;				\
				if (oerr != OMX_ErrorNone) {		\
					fprintf(stderr, #cmd		\
						" failed: %x\n", oerr);	\
					exit(1);			\
				}					\
			} while (0)
/* ... but damn useful.*/

/* Hardware module names: */
#define ENCNAME "OMX.broadcom.video_encode"
#define DECNAME "OMX.broadcom.video_decode"
#define RESIZENAME "OMX.broadcom.resize"

enum states {
	DECINIT,
	DECTUNNELSETUP,
	DECRUNNING,
	DECFLUSH,
	DECDONE,
	DECFAILED,
	ENCPREINIT,
	ENCINIT,
	ENCGOTBUF,
	ENCDONE,
};


/*
static unsigned long mymemory(void)	//prints free memory size 
{
	#include <stdio.h>
	#include <sys/sysinfo.h>

	struct sysinfo myinfo;
	unsigned long total_bytes, free_bytes;

	sysinfo(&myinfo);
	total_bytes = myinfo.mem_unit * myinfo.totalram;
	free_bytes =  myinfo.mem_unit * myinfo.freeram;
//	printf("mem_unit = %lu\n", myinfo.mem_unit);
	printf("total main memory is %lu B, %lu MB\n", total_bytes, total_bytes/1024/1024);
	printf("free ram = %lu B, %lu MB\n", free_bytes, free_bytes/1024/1024);
	return(free_bytes);
}
*/

static struct context {
	AVFormatContext *ic;
	AVFormatContext *oc;
	int		nextin;
	int		nextout;
	int		incount;
	int		outcount;
	volatile int	fps;
	volatile int	framecount;
	int		done;
	volatile int	flags;
	OMX_BUFFERHEADERTYPE *encbufs, *bufhead;
	volatile enum states	decstate;
	volatile enum states	encstate;
	int		encportidx, decportidx, resizeportidx;
	int		fd;
	int		vidindex;
	OMX_HANDLETYPE	m2, m4, resize;
	pthread_mutex_t	lock;
	AVPacket	*nextframe;
	AVPacket	*frameheads[1024];
	AVBitStreamFilterContext *bsfc;
	int		bitrate;
	int		verbose;
	int		width;
	int		height;
} ctx;
#define FLAGS_VERBOSE		(1<<0)
#define FLAGS_DECEMPTIEDBUF	(1<<1)


/* Print some useful information about the state of the port: */
static void dumpport(OMX_HANDLETYPE handle, int port)
{
	OMX_VIDEO_PORTDEFINITIONTYPE	*viddef;
	OMX_PARAM_PORTDEFINITIONTYPE	*portdef;

	MAKEME(portdef, OMX_PARAM_PORTDEFINITIONTYPE);
	portdef->nPortIndex = port;
	OERRq(OMX_GetParameter(handle, OMX_IndexParamPortDefinition, portdef));

	printf("Port %d is %s, %s\n", portdef->nPortIndex,
		(portdef->eDir == 0 ? "input" : "output"),
		(portdef->bEnabled == 0 ? "disabled" : "enabled"));
	printf("Wants %d bufs, needs %d, size %d, enabled: %d, pop: %d, "
		"aligned %d\n", portdef->nBufferCountActual,
		portdef->nBufferCountMin, portdef->nBufferSize,
		portdef->bEnabled, portdef->bPopulated,
		portdef->nBufferAlignment);
	viddef = &portdef->format.video;

	switch (portdef->eDomain) {
	case OMX_PortDomainVideo:
		printf("Video type is currently:\n"
			"\tMIME:\t\t%s\n"
			"\tNative:\t\t%p\n"
			"\tWidth:\t\t%d\n"
			"\tHeight:\t\t%d\n"
			"\tStride:\t\t%d\n"
			"\tSliceHeight:\t%d\n"
			"\tBitrate:\t%d\n"
			"\tFramerate:\t%d (%x); (%f)\n"
			"\tError hiding:\t%d\n"
			"\tCodec:\t\t%d\n"
			"\tColour:\t\t%d\n",
			viddef->cMIMEType, viddef->pNativeRender,
			viddef->nFrameWidth, viddef->nFrameHeight,
			viddef->nStride, viddef->nSliceHeight,
			viddef->nBitrate,
			viddef->xFramerate, viddef->xFramerate,
			((float)viddef->xFramerate/(float)65536),
			viddef->bFlagErrorConcealment,
			viddef->eCompressionFormat, viddef->eColorFormat);
		break;
	case OMX_PortDomainImage:				
		printf("Image type is currently:\n"
			"\tMIME:\t\t%s\n"
			"\tNative:\t\t%p\n"
			"\tWidth:\t\t%d\n"
			"\tHeight:\t\t%d\n"
			"\tStride:\t\t%d\n"
			"\tSliceHeight:\t%d\n"
			"\tError hiding:\t%d\n"
			"\tCodec:\t\t%d\n"
			"\tColour:\t\t%d\n",
			portdef->format.image.cMIMEType,
			portdef->format.image.pNativeRender,
			portdef->format.image.nFrameWidth,
			portdef->format.image.nFrameHeight,	
			portdef->format.image.nStride,
			portdef->format.image.nSliceHeight,
			portdef->format.image.bFlagErrorConcealment,
			portdef->format.image.eCompressionFormat, 
			portdef->format.image.eColorFormat); 		
		break;
/* Feel free to add others. */
	default:
		break;
	}

	free(portdef);
}


static int mapcodec(enum CodecID id)
{
	printf("Mapping codec ID %d (%x)\n", id, id);
	switch (id) {
		case	CODEC_ID_MPEG2VIDEO:
		case	CODEC_ID_MPEG2VIDEO_XVMC:
			return OMX_VIDEO_CodingMPEG2;
		case	CODEC_ID_H264:
			return OMX_VIDEO_CodingAVC;
		default:
			return -1;
	}

	return -1;
}


static void dumpportstate(void)
{
	enum OMX_STATETYPE		state;

	printf("\n\nIn exit handler, after %d frames:\n", ctx.framecount);
	dumpport(ctx.m2, ctx.decportidx);
	dumpport(ctx.m2, ctx.decportidx+1);
	dumpport(ctx.resize, ctx.resizeportidx);
	dumpport(ctx.resize, ctx.resizeportidx+1);
	dumpport(ctx.m4, ctx.encportidx);
	dumpport(ctx.m4, ctx.encportidx+1);

	OMX_GetState(ctx.m2, &state);
	printf("Decoder state: %d\n", state);
	OMX_GetState(ctx.m4, &state);
	printf("Encoder state: %d\n", state);
}



OMX_ERRORTYPE deceventhandler(OMX_HANDLETYPE component,
				struct context *ctx,
				OMX_EVENTTYPE event, 
				OMX_U32 data1,
				OMX_U32 data2,
				OMX_PTR eventdata)
{
	switch (event) {
	case OMX_EventError:
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Decoder %p has errored: %x\n", component, data1);
		return data1;
		break;
	case OMX_EventCmdComplete:
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Decoder %p has completed the last command.\n",
			component);
		break;
	case OMX_EventPortSettingsChanged: {
//	if (ctx->flags & FLAGS_VERBOSE)
		printf("Decoder %p port %d settings changed.\n", component,
			data1);
		if (ctx->verbose) 
			dumpport(component, data1);
		
		ctx->decstate = DECTUNNELSETUP;
	}
		break;
	default:
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Got an event of type %x on decoder %p (d1: %x, d2 %x)\n", event, component, data1, data2);
	}
	return OMX_ErrorNone;
}


OMX_ERRORTYPE resizeeventhandler(OMX_HANDLETYPE component,
				struct context *ctx,
				OMX_EVENTTYPE event, 
				OMX_U32 data1,
				OMX_U32 data2,
				OMX_PTR eventdata)
{
	switch (event) {
	case OMX_EventError:
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Resizer %p has errored: %x\n", component, data1);
		return data1;
		break;
	case OMX_EventCmdComplete:
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Resizer %p has completed the last command.\n",
			component);
		break;
	case OMX_EventPortSettingsChanged: {
//	if (ctx->flags & FLAGS_VERBOSE)
		printf("Resizer %p port %d settings changed.\n", component,
			data1);
		if (ctx->verbose) dumpport(component, data1);
	}
		break;
	default:
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Got an event of type %x on resizer %p (d1: %x, d2 %x)\n", event, component, data1, data2);
	}
	return OMX_ErrorNone;
}




OMX_ERRORTYPE enceventhandler(OMX_HANDLETYPE component,
				struct context *ctx,
				OMX_EVENTTYPE event, 
				OMX_U32 data1,
				OMX_U32 data2,
				OMX_PTR eventdata)
{
	switch (event) {
	case OMX_EventError:
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Encoder %p has errored: %x\n", component, data1);
		return data1;
		break;
	case OMX_EventCmdComplete:
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Encoder %p has completed the last command.\n",
			component);
		break;
	case OMX_EventPortSettingsChanged: {
//	if (ctx->flags & FLAGS_VERBOSE)
		printf("Encoder %p port %d settings changed.\n", component,
			data1);
		if (ctx->verbose) dumpport(component, data1);
	}
		break;
	default:
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Got an event of type %x on encoder %p (d1: %x, d2 %x)\n", event, component, data1, data2);
	}
	return OMX_ErrorNone;
}



OMX_ERRORTYPE emptied(OMX_HANDLETYPE component,
				struct context *ctx,
				OMX_BUFFERHEADERTYPE *buf)
{
	if (ctx->flags & FLAGS_VERBOSE)
		printf("Got a buffer emptied event on component %p, buf %p\n", component, buf);
	buf->nFilledLen = 0;
	ctx->flags |= FLAGS_DECEMPTIEDBUF;
	return OMX_ErrorNone;
}



OMX_ERRORTYPE filled(OMX_HANDLETYPE component,
				struct context *ctx,
				OMX_BUFFERHEADERTYPE *buf)
{
	OMX_BUFFERHEADERTYPE *spare;

	if (ctx->flags & FLAGS_VERBOSE)
		printf("Got buffer %p filled (len %d)\n", buf, buf->nFilledLen);

/*
 * Don't call OMX_FillThisBuffer() here, as the hardware craps out after
 * a short while.  I don't know why.  Reentrancy, or the like, I suspect.
 * Queue the packet(s) and deal with them in main().
 *
 * It only ever seems to ask for the one buffer, but better safe than sorry...
 */

	pthread_mutex_lock(&ctx->lock);
	if (ctx->bufhead == NULL) {
		buf->pAppPrivate = NULL;
		ctx->bufhead = buf;
		pthread_mutex_unlock(&ctx->lock);
		return OMX_ErrorNone;
	}

	spare = ctx->bufhead;
	while (spare->pAppPrivate != NULL)
		spare = spare->pAppPrivate;

	spare->pAppPrivate = buf;
	buf->pAppPrivate = NULL;
	pthread_mutex_unlock(&ctx->lock);

	return OMX_ErrorNone;
}


OMX_CALLBACKTYPE encevents = {
	(void (*)) enceventhandler,
	(void (*)) emptied,
	(void (*)) filled
};

OMX_CALLBACKTYPE decevents = {
	(void (*)) deceventhandler,
	(void (*)) emptied,
	(void (*)) filled
};

OMX_CALLBACKTYPE resizeevents = {
	(void (*)) resizeeventhandler,
	(void (*)) emptied,
	(void (*)) filled
};


static void *fps(void *p)
{
	enum OMX_STATETYPE		state;
	int				lastframe;

	while (1) {
		lastframe = ctx.framecount;
		sleep(1);
		printf("Frame %6d (%5ds).  Frames last second: %d     \r",
			ctx.framecount, ctx.framecount/25,
				ctx.framecount-lastframe);
		fflush(stdout);
		if (0 && ctx.fps == 0) {
			printf("In fps thread, after %d frames:\n",
				ctx.framecount);
			dumpport(ctx.m2, ctx.decportidx);
			dumpport(ctx.m2, ctx.decportidx+1);
			dumpport(ctx.resize, ctx.resizeportidx);
			dumpport(ctx.resize, ctx.resizeportidx+1);
			dumpport(ctx.m4, ctx.encportidx);
			dumpport(ctx.m4, ctx.encportidx+1);

			OMX_GetState(ctx.m2, &state);
			printf("Decoder state: %d\n", state);
			OMX_GetState(ctx.m4, &state);
			printf("Encoder state: %d\n", state);
		}
	}
	return NULL;
}



static OMX_BUFFERHEADERTYPE *allocbufs(OMX_HANDLETYPE h, int port, int enable)
{
	int i;
	OMX_BUFFERHEADERTYPE *list = NULL, **end = &list;
	OMX_PARAM_PORTDEFINITIONTYPE *portdef;

	MAKEME(portdef, OMX_PARAM_PORTDEFINITIONTYPE);
	portdef->nPortIndex = port;
	OERRq(OMX_GetParameter(h, OMX_IndexParamPortDefinition, portdef));

	if (enable)
		OERRq(OMX_SendCommand(h, OMX_CommandPortEnable, port, NULL));

	for (i = 0; i < portdef->nBufferCountActual; i++) {
		OMX_U8 *buf;

		buf = vcos_malloc_aligned(portdef->nBufferSize,
			portdef->nBufferAlignment, "buffer");
//		printf("Allocated a buffer of %d bytes\n", portdef->nBufferSize);
		OERRq(OMX_UseBuffer(h, end, port, NULL, portdef->nBufferSize,
			buf));
		end = (OMX_BUFFERHEADERTYPE **) &((*end)->pAppPrivate);
	}

	free(portdef);

	return list;
}



static AVBitStreamFilterContext *dofiltertest(AVPacket *rp)
{
	AVBitStreamFilterContext *bsfc;

	if (!(rp->data[0] == 0x00 && rp->data[1] == 0x00 &&
		rp->data[2] == 0x00 && rp->data[3] == 0x01)) {
		bsfc = av_bitstream_filter_init("h264_mp4toannexb");
		if (!bsfc) {
			printf("Failed to open filter.  This is bad.\n");
		} else {
			printf("Have a filter at %p\n", bsfc);
		}
	}

	return bsfc;
}



static AVPacket *filter(struct context *ctx, AVPacket *rp)
{
	AVPacket *p;
	AVPacket *fp;
	int rc;

	fp = calloc(sizeof(AVPacket), 1);

	if (ctx->bsfc) {
		rc = av_bitstream_filter_filter(ctx->bsfc,
				ctx->ic->streams[ctx->vidindex]->codec,
				NULL, &(fp->data), &(fp->size),
				rp->data, rp->size,
				rp->flags & AV_PKT_FLAG_KEY);
		if (rc > 0) {
			av_free_packet(rp);
			fp->destruct = av_destruct_packet;
			p = fp;
		} else {
			printf("Failed to filter frame: "
				"%d (%x)\n", rc, rc);
			p = rp;
		}
	} else
		p = rp;

	return p;
}



static void configure(struct context *ctx)
{
	pthread_t	fpst;
	pthread_attr_t	fpsa;
	OMX_CONFIG_FRAMERATETYPE	*framerate;
	OMX_VIDEO_PARAM_PROFILELEVELTYPE *level;
	OMX_VIDEO_PARAM_BITRATETYPE	*bitrate;
	OMX_BUFFERHEADERTYPE		*encbufs;
	OMX_PARAM_PORTDEFINITIONTYPE	*portdef, *portimgdef;
	OMX_VIDEO_PORTDEFINITIONTYPE	*viddef;
	OMX_VIDEO_PARAM_PORTFORMATTYPE	*pfmt;
	OMX_CONFIG_POINTTYPE		*pixaspect;
	int encportidx, decportidx, resizeportidx;
	OMX_HANDLETYPE	m2, m4, resize;

	encportidx = ctx->encportidx;
	decportidx = ctx->decportidx;
	resizeportidx = ctx->resizeportidx;
	m2 = ctx->m2;
	m4 = ctx->m4;
	resize = ctx->resize;

	MAKEME(portdef, OMX_PARAM_PORTDEFINITIONTYPE);
	MAKEME(portimgdef, OMX_PARAM_PORTDEFINITIONTYPE);
	viddef = &portdef->format.video;
	MAKEME(pixaspect, OMX_CONFIG_POINTTYPE);

	printf("Decoder has changed settings.  Setting up resizer.\n");

/*	We need some parameters from de decoder output to put in the resizer:
	eColorFormat (= YUV42-PackedPlanar)
	Width of the frame
	Height of the frame
*/
	portdef->nPortIndex = decportidx+1;
	OERR(OMX_GetParameter(m2, OMX_IndexParamPortDefinition, portdef), ctx->verbose);
	portimgdef->nPortIndex = resizeportidx;

	OERR(OMX_GetParameter(resize, OMX_IndexParamPortDefinition, portimgdef), ctx->verbose);

	portimgdef->format.image.eColorFormat = portdef->format.video.eColorFormat;
	portimgdef->format.image.nFrameWidth = portdef->format.video.nFrameWidth;
	portimgdef->format.image.nFrameHeight = portdef->format.video.nFrameHeight;
	portimgdef->format.image.nStride = 0;
	portimgdef->format.image.nSliceHeight = 0;
	OERR(OMX_SetParameter(resize, OMX_IndexParamPortDefinition, portimgdef), ctx->verbose);

//	The actual resizing if set at call
	if(ctx->width) {
	 	portimgdef->format.image.nFrameWidth = ctx->width;
		portimgdef->format.image.nFrameHeight = ctx->height;
	}
	portimgdef->format.image.nStride = 0;
	portimgdef->format.image.nSliceHeight = 0;
	portimgdef->nPortIndex = resizeportidx+1;
	OERR(OMX_SetParameter(resize, OMX_IndexParamPortDefinition, portimgdef), ctx->verbose);
	free (portimgdef);

/*	Now set the input parameters for the encoder to the scaled height/width */
	portdef->format.video.nFrameWidth = portimgdef->format.image.nFrameWidth;
	portdef->format.video.nFrameHeight = portimgdef->format.image.nFrameHeight;
	portdef->format.video.nStride = 0;
	portdef->format.video.nSliceHeight = 0;
	portdef->nPortIndex = encportidx;
	OERR(OMX_SetParameter(m4, OMX_IndexParamPortDefinition, portdef), ctx->verbose);

/*	setup tunnels */
	OERR(OMX_SetupTunnel(m2, decportidx+1, resize, resizeportidx), ctx->verbose);
	OERR(OMX_SetupTunnel(resize, resizeportidx+1, m4, encportidx), ctx->verbose);

//	OERR(OMX_SendCommand(m2, OMX_CommandStateSet, OMX_StateIdle, NULL), ctx->verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandStateSet, OMX_StateIdle, NULL), ctx->verbose);
	OERR(OMX_SendCommand(m4, OMX_CommandStateSet, OMX_StateIdle, NULL), ctx->verbose);

	viddef = &portdef->format.video;
	if (viddef->nBitrate != 0) {
		viddef->nBitrate *= 3;
		viddef->nBitrate /= 4;
	} else {
		viddef->nBitrate = (1*1024*1024/2);
	}
//		viddef->nBitrate = (2*1024*1024);
//	viddef->nFrameWidth  /= 2;
//	viddef->nFrameHeight /= 2;

	viddef->eCompressionFormat = OMX_VIDEO_CodingAVC;
	viddef->nStride = viddef->nSliceHeight = viddef->eColorFormat = 0;
	portdef->nPortIndex = encportidx+1;
	OERR(OMX_SetParameter(m4, OMX_IndexParamPortDefinition, portdef), ctx->verbose);
	free(portdef);

	MAKEME(bitrate, OMX_VIDEO_PARAM_BITRATETYPE);
	bitrate->nPortIndex = encportidx+1;
	bitrate->eControlRate = OMX_Video_ControlRateVariable;
	bitrate->nTargetBitrate = viddef->nBitrate;
	OERR(OMX_SetParameter(m4, OMX_IndexParamVideoBitrate, bitrate), ctx->verbose);
	free(bitrate);

	MAKEME(pfmt, OMX_VIDEO_PARAM_PORTFORMATTYPE);
	pfmt->nPortIndex = encportidx+1;
	pfmt->nIndex = 0;
	pfmt->eCompressionFormat = OMX_VIDEO_CodingAVC;
	pfmt->eColorFormat = OMX_COLOR_FormatYUV420PackedPlanar;
	pfmt->xFramerate = viddef->xFramerate;

	pixaspect->nPortIndex = encportidx+1;
	pixaspect->nX = 118;
	pixaspect->nY = 81;
	OERR(OMX_SetConfig(m4, OMX_IndexParamBrcmPixelAspectRatio, pixaspect), ctx->verbose);
	free(pixaspect);

//		DUMPPORT(m4, encportidx+1); exit(0);
	pfmt->nPortIndex = encportidx+1;
	pfmt->nIndex = 1;
	pfmt->eCompressionFormat = OMX_VIDEO_CodingAVC;
	pfmt->eColorFormat = 0;
	pfmt->xFramerate = 0; //viddef->xFramerate;
	OERR(OMX_SetParameter(m4, OMX_IndexParamVideoPortFormat, pfmt), ctx->verbose);
	free(pfmt);
	
	MAKEME(framerate, OMX_CONFIG_FRAMERATETYPE);
	framerate->nPortIndex = encportidx+1;
	framerate->xEncodeFramerate = viddef->xFramerate;
	OERR(OMX_SetParameter(m4, OMX_IndexConfigVideoFramerate, framerate), ctx->verbose);
	free(framerate);

#if 0 /* Doesn't seem to apply to video? */
printf("Interlacing: %d\n", ic->streams[vidindex]->codec->field_order);
	if (0 || ic->streams[vidindex]->codec->field_order == AV_FIELD_TT) {
		interlace->nPortIndex = encportidx+1;
		interlace->eMode = OMX_InterlaceFieldsInterleavedUpperFirst;
		interlace->bRepeatFirstField = 0;
		OERR(OMX_SetParameter(m4, OMX_IndexConfigCommonInterlace,
			interlace), ctx->verbose);
	}
#endif

	MAKEME(level, OMX_VIDEO_PARAM_PROFILELEVELTYPE);
	level->nPortIndex = encportidx+1;
	OERR(OMX_GetParameter(m4, OMX_IndexParamVideoProfileLevelCurrent, level), ctx->verbose);
	if (ctx->verbose) printf("Current level:\t\t%d\nCurrent profile:\t%d\n",
		level->eLevel, level->eProfile);
	OERR(OMX_SetParameter(m4, OMX_IndexParamVideoProfileLevelCurrent, level), ctx->verbose);
	free(level);
	ctx->encbufs = encbufs = allocbufs(m4, encportidx+1, 1);
	OERR(OMX_SendCommand(m2, OMX_CommandPortEnable, decportidx+1, NULL), ctx->verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandPortEnable, resizeportidx, NULL), ctx->verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandPortEnable, resizeportidx+1, NULL), ctx->verbose);
	OERR(OMX_SendCommand(m4, OMX_CommandPortEnable, encportidx, NULL), ctx->verbose);
	OERR(OMX_SendCommand(m4, OMX_CommandStateSet, OMX_StateExecuting, NULL), ctx->verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandStateSet, OMX_StateExecuting, NULL), ctx->verbose);
	sleep(1);
	OERR(OMX_FillThisBuffer(m4, encbufs), ctx->verbose);

/* Dump current port states: */
	if (ctx->verbose) {
		dumpport(m2, decportidx);
		dumpport(m2, decportidx+1);
		dumpport(resize, resizeportidx);
		dumpport(resize, resizeportidx+1);
		dumpport(m4, encportidx);
		dumpport(m4, encportidx+1);
	}

	if (ctx->verbose) atexit(dumpportstate);
	pthread_attr_init(&fpsa);
	pthread_attr_setdetachstate(&fpsa, PTHREAD_CREATE_DETACHED);
	pthread_create(&fpst, &fpsa, fps, NULL);
}



static void usage(const char *name)
{
	fprintf(stderr, "Usage: %s [-v] [-b bitrate] [-s WxH] <infile> <outfile>\n\n"
		"Where:\n"
	"\t-v Be (very) verbose\n"
	"\t-b bitrate\tTarget bitrate in bits/second (default: 2Mb/s)\n"
	"\t-s Output Width x Height in multiples of 16 (default: no change from original)\n"
	"\n", name);
	exit(1);
}



int main(int argc, char *argv[])
{
	AVFormatContext	*ic;
	char		*iname;
	char		*oname;
	char		*size;
	int		err;
	int		vidindex;
	int		i, j;
	OMX_ERRORTYPE	oerr;
	OMX_HANDLETYPE	m2 = NULL, m4 = NULL, resize = NULL;
	OMX_VIDEO_PARAM_PORTFORMATTYPE	*pfmt;
	OMX_PORT_PARAM_TYPE		*porttype;
	OMX_PARAM_PORTDEFINITIONTYPE	*portdef;
	OMX_BUFFERHEADERTYPE		*decbufs;
	OMX_VIDEO_PORTDEFINITIONTYPE	*viddef;
	OMX_VIDEO_PARAM_PROFILELEVELTYPE *level;
	int		decportidx = 200;
	int		resizeportidx = 60;
	int		encportidx = 130;
	int		fd;
	time_t		start, end;
	int		offset;
	AVPacket	*p, *rp;
	int		ish264;
	int		filtertest;
	int		opt;
	ILCLIENT_T	*client;

	if (argc < 3)
		usage(argv[0]);

	ctx.bitrate = 2*1024*1024;
	ctx.verbose = 0;
	ctx.width = 0;
	ctx.height = 0;

	while ((opt = getopt(argc, argv, ":b:vs:")) != -1) {
		switch (opt) {
		case 'b':	//bitrate
			ctx.bitrate = atoi(optarg);
			printf("bitrate = %d\n", ctx.bitrate);
			break;
		case 'v':	//verbose`
			ctx.verbose = 1;
			break;
		case 's':	//WxH
			ctx.width = atoi(optarg);
			if ((atoi(optarg) % 16) != 0) {
				printf("W = %d is not a multiple of 16\n", ctx.width);
				usage(argv[0]);
			}
			if (ctx.width <16 || ctx.width > 1080) {
				printf("W = %d should be between 16 and 1080\n", ctx.width);
				usage(argv[0]);
			}	
			printf("W = %d\n", ctx.width);
			if ((size = strchr(optarg, 'x')) == NULL) {
				printf("wrong size parameter (no 'x') exiting\n");
				usage(argv[0]);
			}
			ctx.height = atoi(size+1);
			if ((atoi(size+1) % 16) != 0) {
				printf("H = %d is not a multiple of 16\n", ctx.height);
				usage(argv[0]);
			}
			if (ctx.height <16 || ctx.height > 1080) {
				printf("H = %d should be between 16 and 1080\n", ctx.height);
				usage(argv[0]);
			}	
			printf("H = %d\n", ctx.height);
			break;
		case '?':
			usage(argv[0]);
		}
	}
	if ((client = ilclient_init()) == NULL)
		return -2;
	iname = argv[optind++];
	oname = argv[optind++];

	MAKEME(porttype, OMX_PORT_PARAM_TYPE);
	MAKEME(portdef, OMX_PARAM_PORTDEFINITIONTYPE);
	MAKEME(pfmt, OMX_VIDEO_PARAM_PORTFORMATTYPE);

	av_register_all();

	ic = NULL;
	ish264 = 0;
	pthread_mutex_init(&ctx.lock, NULL);

#if 0
	fmt = av_oformat_next(fmt);
	while (fmt) {
		printf("Found '%s'\t\t'%s'\n", fmt->name, fmt->long_name);
		fmt = av_oformat_next(fmt);
	}
#endif

	/* Input init: */

	if ((err = avformat_open_input(&ic, iname, NULL, NULL) != 0)) {
		fprintf(stderr, "Failed to open '%s': %s\n", iname,
			strerror(err));
		exit(1);
	}
	ctx.ic = ic;

	if (avformat_find_stream_info(ic, NULL) < 0) {
		fprintf(stderr, "Failed to find streams in '%s'\n", iname);
		exit(1);
	}

	av_dump_format(ic, 0, iname, 0);

	vidindex = av_find_best_stream(ic, AVMEDIA_TYPE_VIDEO, -1, -1,
		NULL, 0);
	if (vidindex < 0) {
		fprintf(stderr, "Failed to find a video stream in '%s'\n",
			iname);
		exit(1);
	}
	printf("Found a video at index %d\n", vidindex);

	printf("Frame size: %dx%d\n", ic->streams[vidindex]->codec->width, 
		ic->streams[vidindex]->codec->height);
	ish264 = (ic->streams[vidindex]->codec->codec_id == CODEC_ID_H264);

	/* Output init: */
	ctx.fd = fd = open(oname, O_CREAT | O_LARGEFILE | O_WRONLY | O_TRUNC,
			0666);
	printf("File descriptor %d\n", fd);


#if 0
	avformat_alloc_output_context(&oc, NULL, /*NULL,*/ oname);
	if (!oc) {
		printf("Couldn't determine output from '%s'; using MPEG.\n",
			oname);
		avformat_alloc_output_context(&oc, NULL, /*"matroska",*/ oname);
	}
#endif
//	if (!oc)
//		exit(1);
	
//	fmt = oc->oformat;
	
	for (i = 0; i < ic->nb_streams; i++) {
		printf("Found stream %d, context %p\n",
			ic->streams[i]->index, ic->streams[i]->codec);
	}

	bcm_host_init();
	OERR(OMX_Init(), ctx.verbose);
	OERR(OMX_GetHandle(&m2, DECNAME, &ctx, &decevents), ctx.verbose);
	OERR(OMX_GetHandle(&m4, ENCNAME, &ctx, &encevents), ctx.verbose);
	OERR(OMX_GetHandle(&resize, RESIZENAME, &ctx, &resizeevents), ctx.verbose);
	ctx.m2 = m2;
	ctx.m4 = m4;
	ctx.resize = resize;

	if (ctx.verbose) printf("Obtained handles.  %p decode, %p resize, %p encode\n",
		m2, resize, m4);

	OERR(OMX_GetParameter(m2, OMX_IndexParamVideoInit, porttype), ctx.verbose);
	if (ctx.verbose) printf("Found %d ports, starting at %d (%x) on decoder\n",
		porttype->nPorts, porttype->nStartPortNumber,
		porttype->nStartPortNumber);
	ctx.decportidx = decportidx = porttype->nStartPortNumber;

	OERR(OMX_GetParameter(resize, OMX_IndexParamImageInit, porttype), ctx.verbose);
	if (ctx.verbose) printf("Found %d ports, starting at %d (%x) on resizer\n",
		porttype->nPorts, porttype->nStartPortNumber,
		porttype->nStartPortNumber);
	ctx.resizeportidx = resizeportidx = porttype->nStartPortNumber;

	OERR(OMX_GetParameter(m4, OMX_IndexParamVideoInit, porttype), ctx.verbose);
	if (ctx.verbose) printf("Found %d ports, starting at %d (%x) on encoder\n",
		porttype->nPorts, porttype->nStartPortNumber,
		porttype->nStartPortNumber);
	ctx.encportidx = encportidx = porttype->nStartPortNumber;
	free(porttype);

	OERR(OMX_SendCommand(m2, OMX_CommandPortDisable, decportidx, NULL), ctx.verbose);
	OERR(OMX_SendCommand(m2, OMX_CommandPortDisable, decportidx+1, NULL), ctx.verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandPortDisable, resizeportidx, NULL), ctx.verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandPortDisable, resizeportidx+1, NULL), ctx.verbose);
	OERR(OMX_SendCommand(m4, OMX_CommandPortDisable, encportidx, NULL), ctx.verbose);
	OERR(OMX_SendCommand(m4, OMX_CommandPortDisable, encportidx+1, NULL), ctx.verbose);

	portdef->nPortIndex = decportidx;
	OERR(OMX_GetParameter(m2, OMX_IndexParamPortDefinition, portdef), ctx.verbose);
	viddef = &portdef->format.video;
	viddef->nFrameWidth = ic->streams[vidindex]->codec->width;
	viddef->nFrameHeight = ic->streams[vidindex]->codec->height;
	printf("Mapping codec %d to %d\n",
		ic->streams[vidindex]->codec->codec_id,
		mapcodec(ic->streams[vidindex]->codec->codec_id));
	viddef->eCompressionFormat = 
		mapcodec(ic->streams[vidindex]->codec->codec_id);
	viddef->bFlagErrorConcealment = 0;
//	viddef->xFramerate = 25<<16;
	OERR(OMX_SetParameter(m2, OMX_IndexParamPortDefinition, portdef), ctx.verbose);
	free(portdef);

#if 0
/* It appears these have limited effect: */
	dataunit->nPortIndex = decportidx;
	dataunit->eUnitType = OMX_DataUnitCodedPicture;
	dataunit->eEncapsulationType = OMX_DataEncapsulationGenericPayload;
	OERR(OMX_SetParameter(m2, OMX_IndexParamBrcmDataUnit, dataunit), ctx.verbose);

	if (ish264) {
		naltype->nPortIndex = decportidx;
		naltype->eNaluFormat = OMX_NaluFormatStartCodes;
		OERR(OMX_SetParameter(m2, OMX_IndexParamNalStreamFormatSelect, naltype), ctx.verbose);
	}
#endif

	MAKEME(level, OMX_VIDEO_PARAM_PROFILELEVELTYPE);
	level->nPortIndex = encportidx+1;
/* Dump what the encoder is capable of: */
	if (ctx.verbose) print_codecs();
	if (ctx.verbose) {
		for (oerr = OMX_ErrorNone, i = 0; oerr == OMX_ErrorNone; i++) {
			pfmt->nIndex = i;
			oerr = OMX_GetParameter(m4, OMX_IndexParamVideoPortFormat, pfmt);
			if (oerr == OMX_ErrorNoMore)
				break;
			printf("Codecs supported:\n"
				"\tIndex:\t\t%d\n"
				"\tCodec:\t\t%d (%x)\n"
				"\tColour:\t\t%d\n"
				"\tFramerate:\t%x (%f)\n",
				pfmt->nIndex,
				pfmt->eCompressionFormat, pfmt->eCompressionFormat,
				pfmt->eColorFormat,
				pfmt->xFramerate,
				((float)pfmt->xFramerate/(float)65536));
		}

		for (oerr = OMX_ErrorNone, i = 0; oerr == OMX_ErrorNone; i++) {
			level->nProfileIndex = i;
			oerr = OMX_GetParameter(m4,
				OMX_IndexParamVideoProfileLevelQuerySupported,
				level);
			if (oerr == OMX_ErrorNoMore)
				break;
			printf("Levels supported:\n"
				"\tIndex:\t\t%d\n"
				"\tProfile:\t%d\n"
				"\tLevel:\t\t%d\n",
				level->nProfileIndex,
				level->eProfile,
				level->eLevel);
		}
	}
	free(pfmt);
	free(level);

/* Dump current port states: */
	if (ctx.verbose) {
		dumpport(m2, decportidx);
		dumpport(m2, decportidx+1);
		dumpport(resize, resizeportidx);
		dumpport(resize, resizeportidx+1);
		dumpport(m4, encportidx);
		dumpport(m4, encportidx+1);
	}

	OERR(OMX_SendCommand(m2, OMX_CommandStateSet, OMX_StateIdle, NULL), ctx.verbose);

	decbufs = allocbufs(m2, decportidx, 1);

/* Start the initial loop.  Process until we have a state change on port 131 */
	ctx.decstate = DECINIT;
	ctx.encstate = ENCPREINIT;
	OERR(OMX_SendCommand(m2, OMX_CommandStateSet, OMX_StateExecuting, NULL), ctx.verbose);

	rp = calloc(sizeof(AVPacket), 1);
	filtertest = ish264;

	for (offset = i = j = 0; ctx.decstate != DECFAILED; i++, j++) {
		int rc;
		int k;
		int size, nsize;
		OMX_BUFFERHEADERTYPE *spare;

		if (offset == 0 && ctx.decstate != DECFLUSH) {
			rc = av_read_frame(ic, rp);
			if (rc != 0) {
				if (ic->pb->eof_reached)
					ctx.decstate = DECFLUSH;
				break;
			}
			if (rp->stream_index != vidindex) {
				i--;
				av_free_packet(rp);
				continue;
			}
			size = rp->size;
			ctx.fps++;
			ctx.framecount++;

			if (ish264 && filtertest) {
				filtertest = 0;
				ctx.bsfc = dofiltertest(rp);
			}
			if (ctx.bsfc) {
				p = filter(&ctx, rp);
			} else {
				p = rp;
			}
		}

		switch (ctx.decstate) {
		case DECTUNNELSETUP:
			start = time(NULL);
			//printf("NOW to CONFIGURE !!!!!!!!!!!!!!\n\n");
			configure(&ctx);
			ctx.decstate = DECRUNNING;
			break;
		case DECFLUSH:
			size = 0;
			/* Add the flush code here */
			printf("IN FLUSH NOW\n\n");
			break;
		case DECINIT:
			if (i < 120) /* Bail; decoder doesn't like it */
				break;
			ctx.decstate = DECFAILED;
			/* Drop through */
		case DECFAILED:
			fprintf(stderr, "Failed to set the parameters after "
					"%d video frames.  Giving up.\n", i);
			dumpport(m2, decportidx);
			dumpport(m2, decportidx+1);
			dumpport(resize, resizeportidx);
			dumpport(resize, resizeportidx+1);
			dumpport(m4, encportidx);
			dumpport(m4, encportidx+1);
			exit(1);
			break;
		default:
			break;	/* Shuts the compiler up */
		}

		for (spare = NULL; !spare; usleep(10)) {
			pthread_mutex_lock(&ctx.lock);
			spare = ctx.bufhead;
			ctx.bufhead = NULL;
			ctx.flags &= ~FLAGS_DECEMPTIEDBUF;
			pthread_mutex_unlock(&ctx.lock);
			while (spare) {
				write(fd, &spare->pBuffer[spare->nOffset],
					spare->nFilledLen);
				spare->nFilledLen = 0;
				spare->nOffset = 0;
				OERRq(OMX_FillThisBuffer(m4, spare));
				spare = spare->pAppPrivate;
			}

			spare = decbufs;
			for (k = 0; spare && spare->nFilledLen != 0; k++)
				spare = spare->pAppPrivate;
		}

		if (size > spare->nAllocLen) {
			nsize = spare->nAllocLen;
		} else {
			nsize = size;
		}

		if (ctx.decstate != DECFLUSH) {
			memcpy(spare->pBuffer, &(p->data[offset]), nsize);
			spare->nFlags = i == 0 ? OMX_BUFFERFLAG_STARTTIME : 0;
			spare->nFlags |= size == nsize ?
				OMX_BUFFERFLAG_ENDOFFRAME : 0;
		} else {
			spare->nFlags = OMX_BUFFERFLAG_STARTTIME |
					OMX_BUFFERFLAG_EOS;
		}
		spare->nFilledLen = nsize;
		spare->nOffset = 0;
		OERRq(OMX_EmptyThisBuffer(m2, spare));
		size -= nsize;
		if (size) {
			offset += nsize;
		} else {
			offset = 0;
			av_free_packet(p);
		}
	}

	close(fd);

	end = time(NULL);

	printf("Processed %d frames in %d seconds; %df/s\n",
		ctx.framecount, end-start, (ctx.framecount/(end-start)));

	// flush the encoder
//	OERR(OMX_SendCommand(m4, OMX_CommandFlush, encportidx, NULL), ctx.verbose);
//	OERR(OMX_SendCommand(m4, OMX_CommandFlush, encportidx+ctx.verbose, NULL), ctx.verbose);



	// tear down the tunnels
	OERR(OMX_SendCommand(m2, OMX_CommandStateSet, OMX_StateIdle, NULL), ctx.verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandStateSet, OMX_StateIdle, NULL), ctx.verbose);
	OERR(OMX_SendCommand(m4, OMX_CommandStateSet, OMX_StateIdle, NULL), ctx.verbose);
	OERR(OMX_SendCommand(m2, OMX_CommandStateSet, OMX_StateLoaded, NULL), ctx.verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandStateSet, OMX_StateLoaded, NULL), ctx.verbose);
	OERR(OMX_SendCommand(m4, OMX_CommandStateSet, OMX_StateLoaded, NULL), ctx.verbose);
	// free buffers
	vcos_free(decbufs);
	vcos_free(ctx.encbufs);
	// Apparantly the teardwon function is not implemented. Use setup function instead
	//OERR(OMX_TeardownTunnel(m2, decportidx+ctx.verbose, resize, resizeportidx), ctx.verbose);
	//OERR(OMX_TeardownTunnel(resize, resizeportidx+ctx.verbose, m4, encportidx), ctx.verbose);
	OERR(OMX_SendCommand(m2, OMX_CommandPortDisable, decportidx, NULL), ctx.verbose);
	OERR(OMX_SendCommand(m2, OMX_CommandPortDisable, decportidx+ctx.verbose, NULL), ctx.verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandPortDisable, resizeportidx, NULL), ctx.verbose);
	OERR(OMX_SendCommand(resize, OMX_CommandPortDisable, resizeportidx+ctx.verbose, NULL), ctx.verbose);
	OERR(OMX_SendCommand(m4, OMX_CommandPortDisable, encportidx, NULL), ctx.verbose);
	OERR(OMX_SendCommand(m4, OMX_CommandPortDisable, encportidx+ctx.verbose, NULL), ctx.verbose);
//	ilclient_disable_port_buffers(m2, decportidx, NULL, NULL, NULL);
//	ilclient_disable_port_buffers(m4, encportidx, NULL, NULL, NULL);
	OERR(OMX_SetupTunnel(m2, decportidx+ctx.verbose, NULL, 0), ctx.verbose);
	OERR(OMX_SetupTunnel(resize, resizeportidx, NULL, 0), ctx.verbose);
	OERR(OMX_SetupTunnel(resize, resizeportidx+ctx.verbose, NULL, 0), ctx.verbose);
	OERR(OMX_SetupTunnel(m4, encportidx, NULL, 0), ctx.verbose);
	OERR(OMX_FreeHandle(m2), ctx.verbose);
	OERR(OMX_FreeHandle(resize), ctx.verbose);
	OERR(OMX_FreeHandle(m4), ctx.verbose);

//	free(porttype);
//	free(portdef);
//	free(pfmt);
//	free(level);
	return 0;
}
