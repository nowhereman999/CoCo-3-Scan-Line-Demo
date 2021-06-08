#NAME := DAF_003.asm
NAME := DAF_6_Sprites8.asm
DEBUG := -debug
SPEED := -throttle

# Usage example:
#           make NAME=prog_name.asm. (will assemble and run in debug mode at full speed)
#           make NAME=prog_name.asm DEBUG=-nodebug (will assemble and run at full speed)
#           make NAME=prog_name.asm SPEED=-throttle (will assemble and run in debug at normal speed)
#           make NAME=prog_name.asm DEBUG=-nodebug SPEED=-throttle (will assemble and run at normal speed)

# MAME Trace route command:
# trace output.tr.txt,0,,{tracelog "A=%02X,B=%02X,X=%02X,Y=%02X,U=%02X,S=%02X,CC=%02X ",a,b,x,y,u,s,cc}

test:
	lwasm -9bl -p cd -o./NEW.BIN $(NAME) > Assembly_Listing.txt
#	./slz P NEW.BIN new.slz
#	cat slzloader.bin new.slz > GO.BIN
#	cat bigloader.bin NEW.BIN > GO.BIN

	./CC3SLNV1.2 NEW.BIN -oGO.BIN
	cp BLANK.DSK DISK1.DSK
	imgtool put coco_jvc_rsdos ./Disk1.DSK ./GO.BIN GO.BIN

# Run MAME in normal mode
#	mame -nodebug -window coco3 -flop1 ./DISKDMK1.DSK -throttle -autoboot_command "\n\nLOADM\"GO\n" -ui_active

# Run MAME in debug mode and no throttling (full speed)
#	mame -debug -window coco3 -flop1 ./DISKDMK1.DSK -nothrottle -autoboot_command "\n\nLOADM\"GO\n" -ui_active

	mame $(DEBUG) -window -r 1280x800 coco3 -flop1 ./DISK1.DSK $(SPEED) -autoboot_command "\n\nLOADM\"GO\n" -ui_active

cocoSDC:
# Assemble program
	lwasm -9bl -p cd -o./NEW.BIN $(NAME) > Assembly_Listing.txt

# compress
#	./slz P NEW.BIN new.slz
#	cat slzloader.bin new.slz > GO.BIN

#	cat bigloader.bin NEW.BIN > GO.BIN

	./CC3SLNV1.2 NEW.BIN -oGO.BIN

# Copy to CoCo SDC
	rm -r DISK1.DSK
	cp BLANK.DSK DISK1.DSK
	imgtool put coco_jvc_rsdos ./DISK1.DSK ./GO.BIN GO.BIN
	rm /Volumes/SD16MB/DISK1.DSK
	cp ./DISK1.DSK /Volumes/SD16MB/DISK1.DSK
	diskutil eject /Volumes/SD16MB/
