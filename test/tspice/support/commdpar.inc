! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!     The following parameters are the system parameters required
!     by PERCY.  Be sure to read any notes before adjusting these
!
!
!     The maximum number of commands that can be buffered is
!     determined by the value of MAXCOM.  This parameter is
!     used primarily by NXTCOM.
!
      INTEGER               MAXCOM
      PARAMETER           ( MAXCOM = 20 )


!
!     The parameter FILEN is the maximum length of a file name
!     on a particular system.
!
      INTEGER               FILEN
      PARAMETER           ( FILEN = 128 )

!
!     The parameter COMSIZ is the maximum length allowed for a
!     command.
!
      INTEGER               COMSIZ
      PARAMETER           ( COMSIZ = 1024 )

!
!     The parameter ERRSIZ is the maximum length allowed for
!     error messages.
!
      INTEGER               ERRSIZ
      PARAMETER           ( ERRSIZ = 1760 )
!
!     The parameter STYSIZ is the maximum length expected for
!     a NICEPR style string.
!
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
