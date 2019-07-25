fpc -n -O3 -Si -Sc -Sg -Xd -CX -XXs -Rintel -Pi386 -MObjFPC $(dirname $0)/rain.lpr
upx $(dirname $0)/rain
rm $(dirname $0)/*.o $(dirname $0)/*.ppu