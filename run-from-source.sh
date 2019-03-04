#! /bin/bash

MONODEVELOP_CONSOLE_LOG_LEVEL=All MONODEVELOP_DEV_ADDINS=$(pwd)/XSVim/bin/Debug /Applications/Visual\ Studio.app/Contents/MacOS/VisualStudio --no-redirect XSVim.sln
