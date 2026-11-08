!$Procedure T_DAFOPN ( Open a new DAF file for testing )

      SUBROUTINE T_DAFOPN ( FNAME, OUTBFF, UNIT )

! $ Abstract
!
!     Open a new DAF file.
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
!     None.
!
! $ Keywords
!
!     TEST ROUTINE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      CHARACTER*(*)         FNAME
      INTEGER               OUTBFF
      INTEGER               UNIT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of the DAF to open.
!     OUTBFF     I   Binary file format code used to create file.
!     UNIT       O   Logical unit used to open the file.
!
! $ Detailed_Input
!
!     FNAME      is the name of the DAF that is to be created.
!
!     OUTBFF     is an integer code that indicates the binary file
!                format targeted for OUTPUT.  Acceptable values
!                are the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.
!
! $ Detailed_Output
!
!     UNIT       is the logical unit used to open FNAME.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This module opens FNAME with NEW status.
!
! $ Exceptions
!
!     1) SPICE(FILEOPENFAILED) is signaled if the OPEN statement fails.
!
! $ Particulars
!
!     This routine simply opens a new DAF and returns the logical
!     unit associated with the DAF.
!
!     Note: This code is not a model of perfection.  It is hacked
!     together from pieces of code written elsewhere to support
!     creation of non-native binaries for testing purposes only.
!
! $ Examples
!
!     See F_ZZDGDR for usage.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 28-OCT-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               RTRIM
      LOGICAL               RETURN

!
!     Local Variables
!
      INTEGER               IOSTAT

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_DAFOPN' )
      END IF

!
!     First kill FNAME.
!
      CALL KILFIL ( FNAME )

!
!     Now fetch a logical unit.
!
      CALL GETLUN ( UNIT )

!
!     Open the file.
!
      OPEN ( UNIT   = UNIT, &
             FILE   = FNAME(1:RTRIM(FNAME)), &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'NEW', &
             IOSTAT = IOSTAT                 )

!
!     Check IOSTAT.
!
      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Unable to open file, #.  IOSTAT was #.' )
         CALL ERRCH  ( '#', FNAME                               )
         CALL ERRINT ( '#', IOSTAT                              )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                  )
         CALL CHKOUT ( 'T_DAFOPN'                               )
         RETURN
      END IF

      CALL CHKOUT ( 'T_DAFOPN' )
      RETURN

      END
