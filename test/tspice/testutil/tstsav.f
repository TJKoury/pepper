C$Procedure      TSTSAV ( Save test info )
C
      SUBROUTINE TSTSAV ( ENV, VERSN, TIME )
C
C$ Abstract
C
C     Keep the global test information available for other parts
C     of the test system
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C      None.
C
C$ Keywords
C
C       TESTING
C
C$ Declarations
C
      IMPLICIT NONE
      CHARACTER*(*)         ENV
      CHARACTER*(*)         VERSN
      CHARACTER*(*)         TIME
 
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ENV       I/O   is the environment in which the test is performed
C      VERSN     I/O   is the version of the test program
C      TIME      I/O   is the date-time timestamp for the test program.
C
C$ Detailed_Input
C
C     ENV        is the environment in which the test is performed.
C
C     VERSN      is the version of the test program being executed.
C
C     TIME       is the time at which the program begain.
C
C$ Detailed_Output
C
C     ENV        is the environment in which the test is performed.
C
C     VERSN      is the version of the test program being executed.
C
C     TIME       is the time at which the program begain.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This is simply a storage routine to allow various portions of
C     the test program to retrieve the various global attributes
C     of a test program.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    Testing Utilities Version 1.0.0, 4-NOV-1994 (WLT)
C
C
C-&
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
 
C
C     The entry point below fetches the saved values of ENV, VERSN,
C     and TIME.
C
      ENTRY TSTGET ( ENV, VERSN, TIME )
 
 
      ENV   = MYENV
      VERSN = MYVER
      TIME  = MYTIME
      RETURN
      
      
C
C     This entry point allows you to save the name of the test log file.
C     
      ENTRY TSTSLF ( ENV )
      
         MYFILE = ENV
         RETURN
         
C
C     This entry point allows you to retrieve the name of the test log
C     file.
C
      ENTRY TSTGLF ( ENV )
      
         ENV = MYFILE
         RETURN      
 
      END
 
 
