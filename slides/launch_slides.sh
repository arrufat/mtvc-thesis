#!/bin/bash


#TRANS=PageTurn
#TRANS=PagePeel
#TRANS=ZoomOutIn
#TRANS=SlideUp
#TRANS=FadeOutFadeIn
TRANS=None
FILE=$1

impressive \
    --nologo \
    --fade \
    --transtime 500 \
    --mousedelay 0 \
    --fontsize 24 \
    --zoom 5 \
    -t $TRANS $FILE
