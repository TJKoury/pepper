!$Procedure ZZPLTCHK ( Private --- Platform Check )

      SUBROUTINE ZZPLTCHK ( OK )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Validate the runtime environment against values assumed by the
!     current toolkit source package.
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
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   Logical indicating the runtime environment is ok.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical when set to .TRUE. indicates that the
!                runtime environment passes any checks implemented
!                by this routine against the configured code.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) This routine signals SPICE(BUG) if it determines the runtime
!        environment is incompatible with the configured binary file
!        format.
!
! $ Particulars
!
!     This routine encapsulates a series of checks to diagnose the
!     runtime environment against assumptions configured in the
!     source code.  Configuration errors are reported via the error
!     SPICE(BUG).
!
! $ Examples
!
!     See ZZDDHOPN for sample usage.
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
!-    SPICELIB Version 1.0.0, 07-AUG-2002 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

!
!     Local Variables
!
      CHARACTER*(WDSIZE)    STRBFF
      CHARACTER*(WDSIZE)    VALUE
      CHARACTER*(WDSIZE)    RTEBFF

      INTEGER               BFF

!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZPLTCHK' )
      END IF

!
!     Verify that the runtime environment's binary file format agrees
!     with the value listed in ZZPLATFM.
!
      CALL ZZPLATFM ( 'FILE_FORMAT', STRBFF )

!
!     Determine what the runtime environment binary file format appears
!     to be.
!
      CALL ZZGETBFF ( BFF )
      CALL ZZDDHGSD ( 'BFF', BFF, RTEBFF )

!
!     Check results, signal SPICE(BUG) if a discrepancy appears.
!
      IF ( STRBFF .NE. RTEBFF ) THEN

         CALL SETMSG   ( 'This version of SPICELIB was originally ' &
         //              'packaged by NAIF for # hardware using ' &
         //              '# with the # compiler.  This ' &
         //              'environment has a binary ' &
         //              'file format of #; however the software is ' &
         //              'running on an environment that has a binary ' &
         //              'file format of #.  This is a severe problem ' &
         //              'and may be because the software package was ' &
         //              'intended for use on a different computer ' &
         //              'system.  It also may be the result of an ' &
         //              'improper port; please contact NAIF.'         )

         CALL ZZPLATFM ( 'SYSTEM',   VALUE                             )
         CALL ERRCH    ( '#',        VALUE                             )

         CALL ZZPLATFM ( 'O/S',      VALUE                             )
         CALL ERRCH    ( '#',        VALUE                             )

         CALL ZZPLATFM ( 'COMPILER', VALUE                             )
         CALL ERRCH    ( '#',        VALUE                             )

         CALL ERRCH    ( '#',        STRBFF                            )

         IF ( RTEBFF .EQ. ' ' ) THEN
            CALL ERRCH ( '#', 'UNKNOWN' )
         ELSE
            CALL ERRCH ( '#',  RTEBFF   )
         END IF

         CALL SIGERR   ( 'SPICE(BUG)'                                  )
         CALL CHKOUT   ( 'ZZPLTCHK'                                    )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZPLTCHK' )
      RETURN

      END
