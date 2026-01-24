.POSIX:

MKDIR  = /bin/mkdir
RM     = /bin/rm
PANDOC = pandoc
PFLAGS = --standalone --to man --variable pandoc-version=
SRCDIR = .
MANDIR = ../man

.PHONY: all clean mandoc

all: man

man:
	$(MKDIR) -p $(MANDIR)
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmapi.1    $(SRCDIR)/dmapi.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmbackup.1 $(SRCDIR)/dmbackup.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmbeat.1   $(SRCDIR)/dmbeat.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmbot.1    $(SRCDIR)/dmbot.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmcamera.1 $(SRCDIR)/dmcamera.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmdb.1     $(SRCDIR)/dmdb.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmdbctl.1  $(SRCDIR)/dmdbctl.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmdwd.1    $(SRCDIR)/dmdwd.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmexport.1 $(SRCDIR)/dmexport.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmfeed.1   $(SRCDIR)/dmfeed.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmfs.1     $(SRCDIR)/dmfs.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmgrc.1    $(SRCDIR)/dmgrc.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmimport.1 $(SRCDIR)/dmimport.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dminfo.1   $(SRCDIR)/dminfo.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dminit.1   $(SRCDIR)/dminit.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmlog.1    $(SRCDIR)/dmlog.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmlogger.1 $(SRCDIR)/dmlogger.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmlua.1    $(SRCDIR)/dmlua.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmmb.1     $(SRCDIR)/dmmb.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmmbctl.1  $(SRCDIR)/dmmbctl.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmpipe.1   $(SRCDIR)/dmpipe.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmplot.1   $(SRCDIR)/dmplot.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmrecv.1   $(SRCDIR)/dmrecv.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmreport.1 $(SRCDIR)/dmreport.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmsend.1   $(SRCDIR)/dmsend.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmserial.1 $(SRCDIR)/dmserial.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmsync.1   $(SRCDIR)/dmsync.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmsystem.1 $(SRCDIR)/dmsystem.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmupload.1 $(SRCDIR)/dmupload.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmuuid.1   $(SRCDIR)/dmuuid.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmved.1    $(SRCDIR)/dmved.1.md
	$(PANDOC) $(PFLAGS) -o $(MANDIR)/dmweb.1    $(SRCDIR)/dmweb.1.md

clean:
	$(RM) -rf $(MANDIR)/*.1
