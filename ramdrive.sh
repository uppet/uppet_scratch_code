DEVNAME=$(hdiutil attach -nomount ram://$((2 * 1024 * 1024 * $1)))
diskutil eraseVolume HFS+ RAMDisc $DEVNAME 

