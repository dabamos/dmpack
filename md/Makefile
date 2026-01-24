.POSIX:

PANDOC = pandoc
FLAGS  = --standalone --to man --variable pandoc-version=
MKDIR  = /bin/mkdir
RM     = /bin/rm
SRCDIR = .
MANDIR = ../man

.PHONY: all clean man

all: man

man:
	$(MKDIR) -p $(MANDIR)
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmapi.1    $(SRCDIR)/dmapi.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmbackup.1 $(SRCDIR)/dmbackup.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmbeat.1   $(SRCDIR)/dmbeat.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmbot.1    $(SRCDIR)/dmbot.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmcamera.1 $(SRCDIR)/dmcamera.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmdb.1     $(SRCDIR)/dmdb.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmdbctl.1  $(SRCDIR)/dmdbctl.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmdwd.1    $(SRCDIR)/dmdwd.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmexport.1 $(SRCDIR)/dmexport.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmfeed.1   $(SRCDIR)/dmfeed.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmfs.1     $(SRCDIR)/dmfs.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmgrc.1    $(SRCDIR)/dmgrc.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmimport.1 $(SRCDIR)/dmimport.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dminfo.1   $(SRCDIR)/dminfo.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dminit.1   $(SRCDIR)/dminit.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmlog.1    $(SRCDIR)/dmlog.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmlogger.1 $(SRCDIR)/dmlogger.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmlua.1    $(SRCDIR)/dmlua.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmmb.1     $(SRCDIR)/dmmb.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmmbctl.1  $(SRCDIR)/dmmbctl.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmpipe.1   $(SRCDIR)/dmpipe.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmplot.1   $(SRCDIR)/dmplot.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmrecv.1   $(SRCDIR)/dmrecv.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmreport.1 $(SRCDIR)/dmreport.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmsend.1   $(SRCDIR)/dmsend.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmserial.1 $(SRCDIR)/dmserial.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmsync.1   $(SRCDIR)/dmsync.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmsystem.1 $(SRCDIR)/dmsystem.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmupload.1 $(SRCDIR)/dmupload.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmuuid.1   $(SRCDIR)/dmuuid.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmved.1    $(SRCDIR)/dmved.1.md
	$(PANDOC) $(FLAGS) -o $(MANDIR)/dmweb.1    $(SRCDIR)/dmweb.1.md

clean:
	$(RM) -rf $(MANDIR)/*.1
