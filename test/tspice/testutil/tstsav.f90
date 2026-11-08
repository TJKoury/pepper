!$Procedure      TSTSAV ( Save test info )
!
      SUBROUTINE TSTSAV ( ENV, VERSN, TIME )
!
! $ Abstract
!
!     Keep the global test information available for other parts
!     of the test system
!
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
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations
!
      IMPLICIT NONE
      CHARACTER*(*)         ENV
      CHARACTER*(*)         VERSN
      CHARACTER*(*)         TIME

!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      ENV       I/O   is the environment in which the test is performed
!      VERSN     I/O   is the version of the test program
!      TIME      I/O   is the date-time timestamp for the test program.
!
! $ Detailed_Input
!
!     ENV        is the environment in which the test is performed.
!
!     VERSN      is the version of the test program being executed.
!
!     TIME       is the time at which the program begain.
!
! $ Detailed_Output
!
!     ENV        is the environment in which the test is performed.
!
!     VERSN      is the version of the test program being executed.
!
!     TIME       is the time at which the program begain.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This is simply a storage routine to allow various portions of
!     the test program to retrieve the various global attributes
!     of a test program.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-    Testing Utilities Version 1.0.0, 4-NOV-1994 (WLT)
!
!
!-&
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    MYVER
      CHARACTER*(LNSIZE)    MYENV
      CHARACTER*(LNSIZE)    MYTIME
      CHARACTER*(FILSIZ)    MYFILE
      SAVE

      DATA                  MYVER   / ' ' /
      DATA                  MYENV   / ' ' /
      DATA                  MYTIME  / ' ' /
      DATA                  MYFILE  / ' ' /



      MYENV  = ENV
      MYVER  = VERSN
      MYTIME = TIME
      RETURN

!
!     The entry point below fetches the saved values of ENV, VERSN,
!     and TIME.
!
      ENTRY TSTGET ( ENV, VERSN, TIME )


      ENV   = MYENV
      VERSN = MYVER
      TIME  = MYTIME
      RETURN


!
!     This entry point allows you to save the name of the test log file.
!
      ENTRY TSTSLF ( ENV )

         MYFILE = ENV
         RETURN

!
!     This entry point allows you to retrieve the name of the test log
!     file.
!
      ENTRY TSTGLF ( ENV )

         ENV = MYFILE
         RETURN

      END


