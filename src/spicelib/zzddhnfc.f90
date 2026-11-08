!$Procedure ZZDDHNFC ( DDH, return native BFF format code )

      SUBROUTINE ZZDDHNFC ( NATBFF )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the integer code for the native binary file format
!     of the host system.
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
!     DAF
!     DAS
!
! $ Keywords
!
!     ASSIGNMENT
!     DAS
!     FILES
!
! $ Declarations

      IMPLICIT NONE
      INCLUDE 'zzddhman.inc'

      INTEGER               NATBFF

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NATBFF     I   Native binary file format code.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     NATBFF         is the native binary file format code for the host
!                    system on which this routine is called.
!
! $ Parameters
!
!     See the INCLUDE file zzddhman.inc for a description of
!     binary file format codes.
!
! $ Exceptions
!
!     1)  If an error occurs while attempting to obtain the host
!         binary file format code, the error will be diagnosed by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine centralizes the logic needed to obtain the host
!     system's binary file format code.
!
! $ Examples
!
!     See usage in DASGRD.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-FEB-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     return native binary file format code
!
!-&


!
!     SPICELIB functions
!
      INTEGER               ISRCHC
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR

      INTEGER               I
      INTEGER               SAVBFF

      LOGICAL               FIRST

!
!     Saved variables
!
      SAVE                  FIRST
      SAVE                  SAVBFF
      SAVE                  STRBFF

!
!     Initial values
!
      DATA                  FIRST / .TRUE. /


!
!     This routine checks in on the first pass only.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF


      IF ( FIRST ) THEN

         CALL CHKIN ( 'ZZDDHNFC' )

!
!        Populate STRBFF, the buffer that contains the labels
!        for each binary file format.
!
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO

!
!        Fetch the native binary file format and determine its
!        integer code.
!
         CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
         CALL UCASE    ( TMPSTR,        TMPSTR )

         SAVBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )

         IF ( SAVBFF .EQ. 0 ) THEN

            CALL SETMSG ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            CALL ERRCH  ( '#', TMPSTR                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'ZZDDHNFC'                                   )
            RETURN

         END IF

!
!        Do not perform initialization tasks again.
!
         FIRST = .FALSE.

         CALL CHKOUT ( 'ZZDDHNFC' )

      END IF


      NATBFF = SAVBFF

      RETURN
      END

