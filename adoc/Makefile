.POSIX:

ADOC    = asciidoctor
MAN     = man
MANDOC  = mandoc
PS2PDF  = ps2pdf

ADOCDIR = .
MANDIR  = ../man
HTMLDIR = $(MANDIR)/html
PDFDIR  = $(MANDIR)/pdf
CSSDIR  = ../share

.PHONY: all clean html man pdf

all: man pdf html

man:
	mkdir -p $(MANDIR)
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmapi.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmbackup.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmbeat.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmbot.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmcamera.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmdb.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmdbctl.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmdwd.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmexport.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmfeed.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmfs.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmgrc.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmimport.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dminfo.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dminit.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmlog.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmlogger.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmlua.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmmb.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmmbctl.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmpipe.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmplot.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmrecv.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmreport.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmsend.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmserial.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmsync.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmsystem.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmuuid.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmved.adoc
	$(ADOC) -b manpage -D $(MANDIR) $(ADOCDIR)/dmweb.adoc

pdf:
	mkdir -p $(PDFDIR)
	$(MAN) -t $(MANDIR)/dmapi.1    | $(PS2PDF) - $(PDFDIR)/dmapi.pdf
	$(MAN) -t $(MANDIR)/dmbackup.1 | $(PS2PDF) - $(PDFDIR)/dmbackup.pdf
	$(MAN) -t $(MANDIR)/dmbeat.1   | $(PS2PDF) - $(PDFDIR)/dmbeat.pdf
	$(MAN) -t $(MANDIR)/dmbot.1    | $(PS2PDF) - $(PDFDIR)/dmbot.pdf
	$(MAN) -t $(MANDIR)/dmcamera.1 | $(PS2PDF) - $(PDFDIR)/dmcamera.pdf
	$(MAN) -t $(MANDIR)/dmdb.1     | $(PS2PDF) - $(PDFDIR)/dmdb.pdf
	$(MAN) -t $(MANDIR)/dmdbctl.1  | $(PS2PDF) - $(PDFDIR)/dmdbctl.pdf
	$(MAN) -t $(MANDIR)/dmdwd.1    | $(PS2PDF) - $(PDFDIR)/dmdwd.pdf
	$(MAN) -t $(MANDIR)/dmexport.1 | $(PS2PDF) - $(PDFDIR)/dmexport.pdf
	$(MAN) -t $(MANDIR)/dmfeed.1   | $(PS2PDF) - $(PDFDIR)/dmfeed.pdf
	$(MAN) -t $(MANDIR)/dmfs.1     | $(PS2PDF) - $(PDFDIR)/dmfs.pdf
	$(MAN) -t $(MANDIR)/dmgrc.1    | $(PS2PDF) - $(PDFDIR)/dmgrc.pdf
	$(MAN) -t $(MANDIR)/dmimport.1 | $(PS2PDF) - $(PDFDIR)/dmimport.pdf
	$(MAN) -t $(MANDIR)/dminfo.1   | $(PS2PDF) - $(PDFDIR)/dminfo.pdf
	$(MAN) -t $(MANDIR)/dminit.1   | $(PS2PDF) - $(PDFDIR)/dminit.pdf
	$(MAN) -t $(MANDIR)/dmlog.1    | $(PS2PDF) - $(PDFDIR)/dmlog.pdf
	$(MAN) -t $(MANDIR)/dmlogger.1 | $(PS2PDF) - $(PDFDIR)/dmlogger.pdf
	$(MAN) -t $(MANDIR)/dmlua.1    | $(PS2PDF) - $(PDFDIR)/dmlua.pdf
	$(MAN) -t $(MANDIR)/dmmb.1     | $(PS2PDF) - $(PDFDIR)/dmmb.pdf
	$(MAN) -t $(MANDIR)/dmmbctl.1  | $(PS2PDF) - $(PDFDIR)/dmmbctl.pdf
	$(MAN) -t $(MANDIR)/dmpipe.1   | $(PS2PDF) - $(PDFDIR)/dmpipe.pdf
	$(MAN) -t $(MANDIR)/dmplot.1   | $(PS2PDF) - $(PDFDIR)/dmplot.pdf
	$(MAN) -t $(MANDIR)/dmrecv.1   | $(PS2PDF) - $(PDFDIR)/dmrecv.pdf
	$(MAN) -t $(MANDIR)/dmreport.1 | $(PS2PDF) - $(PDFDIR)/dmreport.pdf
	$(MAN) -t $(MANDIR)/dmsend.1   | $(PS2PDF) - $(PDFDIR)/dmsend.pdf
	$(MAN) -t $(MANDIR)/dmserial.1 | $(PS2PDF) - $(PDFDIR)/dmserial.pdf
	$(MAN) -t $(MANDIR)/dmsync.1   | $(PS2PDF) - $(PDFDIR)/dmsync.pdf
	$(MAN) -t $(MANDIR)/dmsystem.1 | $(PS2PDF) - $(PDFDIR)/dmsystem.pdf
	$(MAN) -t $(MANDIR)/dmuuid.1   | $(PS2PDF) - $(PDFDIR)/dmuuid.pdf
	$(MAN) -t $(MANDIR)/dmved.1    | $(PS2PDF) - $(PDFDIR)/dmved.pdf
	$(MAN) -t $(MANDIR)/dmweb.1    | $(PS2PDF) - $(PDFDIR)/dmweb.pdf

html:
	mkdir -p $(HTMLDIR)
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmapi.1    > $(HTMLDIR)/dmapi.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmbackup.1 > $(HTMLDIR)/dmbackup.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmbeat.1   > $(HTMLDIR)/dmbeat.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmbot.1    > $(HTMLDIR)/dmbot.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmcamera.1 > $(HTMLDIR)/dmcamera.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmdb.1     > $(HTMLDIR)/dmdb.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmdbctl.1  > $(HTMLDIR)/dmdbctl.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmdwd.1    > $(HTMLDIR)/dmdwd.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmexport.1 > $(HTMLDIR)/dmexport.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmfeed.1   > $(HTMLDIR)/dmfeed.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmfs.1     > $(HTMLDIR)/dmfs.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmgrc.1    > $(HTMLDIR)/dmgrc.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmimport.1 > $(HTMLDIR)/dmimport.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dminfo.1   > $(HTMLDIR)/dminfo.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dminit.1   > $(HTMLDIR)/dminit.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmlog.1    > $(HTMLDIR)/dmlog.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmlogger.1 > $(HTMLDIR)/dmlogger.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmlua.1    > $(HTMLDIR)/dmlua.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmmb.1     > $(HTMLDIR)/dmmb.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmmbctl.1  > $(HTMLDIR)/dmmbctl.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmpipe.1   > $(HTMLDIR)/dmpipe.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmplot.1   > $(HTMLDIR)/dmplot.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmrecv.1   > $(HTMLDIR)/dmrecv.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmreport.1 > $(HTMLDIR)/dmreport.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmsend.1   > $(HTMLDIR)/dmsend.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmserial.1 > $(HTMLDIR)/dmserial.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmsync.1   > $(HTMLDIR)/dmsync.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmsystem.1 > $(HTMLDIR)/dmsystem.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmuuid.1   > $(HTMLDIR)/dmuuid.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmved.1    > $(HTMLDIR)/dmved.html
	$(MANDOC) -Ostyle=mandoc.css -Thtml $(MANDIR)/dmweb.1    > $(HTMLDIR)/dmweb.html
	if [ ! -e $(HTMLDIR)/mandoc.css ]; then cp $(CSSDIR)/mandoc.css $(HTMLDIR); fi

clean:
	if [ `ls -1 $(MANDIR)/*.1 2>/dev/null     | wc -l` -gt 0 ]; then rm $(MANDIR)/*.1; fi
	if [ `ls -1 $(HTMLDIR)/*.html 2>/dev/null | wc -l` -gt 0 ]; then rm $(HTMLDIR)/*.html; fi
	if [ `ls -1 $(HTMLDIR)/*.css 2>/dev/null  | wc -l` -gt 0 ]; then rm $(HTMLDIR)/*.css; fi
	if [ `ls -1 $(PDFDIR)/*.pdf 2>/dev/null   | wc -l` -gt 0 ]; then rm $(PDFDIR)/*.pdf; fi
