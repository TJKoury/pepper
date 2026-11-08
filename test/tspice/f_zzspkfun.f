C$Procedure F_ZZSPKFUN ( Family of tests for SPK function routines )
 
      SUBROUTINE F_ZZSPKFUN ( OK )
 
C$ Abstract
C
C     This routine performs a series of tests on the private
C     SPK routines that support a target or an observer whose
C     ephemeris is given by a subroutine rather than by SPK data.
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB private, function-based
C     SPK routines
C
C        ZZSPKFAO
C        ZZSPKFAP
C        ZZSPKFAT
C        ZZSPKFLT
C        ZZSPKFZO
C        ZZSPKFZT
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C
C-    TSPICE Version 2.1.0, 14-OCT-2021 (BVS)
C
C        Bug fix: fixed routine name in CHKIN/CHKOUT calls
C        in entry F_ZZSPKGET (F_SPKGET -> F_ZZSPKGET).
C
C-    TSPICE Version 2.0.0, 10-OCT-2013 (NJB)
C
C        Various test case names were expanded to be more
C        meaningful. Tolerances were loosened to enable
C        tests to pass in some C environments.
C
C-    TSPICE Version 1.0.0, 10-JAN-2012 (NJB)
C
C-&
  
 
 
C
C     Test Utility Functions
C
      EXTERNAL              F_ZZSPKGET
 
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      JYEAR

C
C     Local parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT  = 1.D-13 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )
      
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 10 )

      INTEGER               CORLEN
      PARAMETER           ( CORLEN = 25 )

      INTEGER               NFRAME
      PARAMETER           ( NFRAME = 5 )

C
C     Local Variables
C
 
      CHARACTER*(CORLEN)    ABCORR ( NCORR )

      CHARACTER*(LNSIZE)    ERROR
      CHARACTER*(FRNMLN)    FRAMES ( NFRAME )
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(LNSIZE)    TITLE      
 
      DOUBLE PRECISION      ACCOBS ( 3 )
      DOUBLE PRECISION      CLT
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      ELT
      DOUBLE PRECISION      ESTATE ( 6 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ISTATE ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      OBSSSB ( 6, 2 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STOBS  ( 6 )
      DOUBLE PRECISION      TSTATE ( 6 )
      DOUBLE PRECISION      XDLT
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               CTRCDE
      INTEGER               CORIDX
      INTEGER               FRMIDX
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               OBSCDE
      INTEGER               OUTCTR
      INTEGER               TIMIDX
      INTEGER               TRGCDE

C
C     Saved variables
C
      SAVE                  ABCORR
      SAVE                  FRAMES

C
C     Initial values
C
      DATA                  ABCORR / ' nOne',
     .                               ' lT',
     .                               ' Lt + s',
     .                               '  Cn',
     .                               'cN + S',
     .                               ' xlT',
     .                               ' XLt + s',
     .                               '  xCn',
     .                               'XcN + S'   /

      DATA                  FRAMES / 'J2000',      'MARSIAU', 
     .                               'IAU_EARTH',  'IAU_MARS',
     .                               'IAU_JUPITER'            /


 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZSPKFUN' )
 
C*****************************************************************
C*                                                               *
C*                                                               *
C*    Set-up                                                     *
C*                                                               *
C*                                                               *
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C

C
C     Create and load SPK and PCK.
C
      CALL TCASE  ( 'Create kernels.' )

      CALL KILFIL ( 'spkfun.bsp' )
      CALL KILFIL ( 'spkfun.ker' )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL TSTSPK ( 'spkfun.bsp', .TRUE.,  HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL TSTPCK ( 'spkfun.ker', .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TPARSE ( '1 JAN 1995',   ET,     ERROR )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

C
C --- Case: ------------------------------------------------------
C

C
C     Exercise the test utility routines located at the bottom
C     of this file.
C
      CALL TCASE ( 'Exercise the local SPK set/get utilities.' )

      TRGCDE = 399
      CTRCDE = 3

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )

      ET  = 10 * JYEAR()
      REF = 'IAU_EARTH'

      CALL SPKGEO ( TRGCDE, ET, REF, CTRCDE, XSTATE, XLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL F_ZZSPKGET ( ET, REF, OUTCTR, STATE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the correct center of motion is returned.
C
      CALL CHCKSI ( 'Center', OUTCTR, '=', CTRCDE, 0, OK )

C
C     Check the output state. We expect exact equality.
C
      CALL CHCKAD ( 'Position', STATE,     '=', 
     .                          XSTATE,    3,     0.D0, OK )

      CALL CHCKAD ( 'Velocity', STATE(4),  '=', 
     .                          XSTATE(4), 3,     0.D0, OK )



C*****************************************************************
C*                                                               *
C*                                                               *
C*    ZZSPKFZT tests                                             *
C*                                                               *
C*                                                               *
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad aberration correction.'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFZT ( F_ZZSPKGET,   ET,     REF, 
     .                'L+S',        OBSCDE, STATE, LT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad frame name'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J200'

      ET  = JYEAR()

      CALL ZZSPKFZT ( F_ZZSPKGET,   ET,     REF, 
     .                'LT+S',       OBSCDE, STATE, LT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target data.'  )

      TRGCDE = 777
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFZT ( F_ZZSPKGET,   ET,     REF, 
     .                'LT+S',       OBSCDE, STATE, LT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target center data.'  )

      TRGCDE = 7
      CTRCDE = 777
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFZT ( F_ZZSPKGET,   ET,     REF, 
     .                'LT+S',       OBSCDE, STATE, LT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no observer data.'  )

      TRGCDE = 5
      CTRCDE = 3
      OBSCDE = 444

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFZT ( F_ZZSPKGET,   ET,     REF, 
     .                'LT+S',       OBSCDE, STATE, LT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C
C     ZZSPKFZT normal cases:
C
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFZT for every aberration correction. '
     .//           'Use different frames. Target = Earth. Center = '
     .//           'Earth-Moon barycenter. Observer = Mars.'        )
     
      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 499

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()

C
C --- Case: ------------------------------------------------------
C               
               TITLE = 'ZZSPKFZT: Target = #; Observer = #; Ref = # '
     .         //      'Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                      OBSCDE, XSTATE, XLT                  ) 
               CALL CHCKXC ( .FALSE., ' ', OK )
   

               CALL ZZSPKFZT ( F_ZZSPKGET,     ET,     REF, 
     .                         ABCORR(CORIDX), OBSCDE, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFZT: change observer, target, '
     .//           'and center, and repeat previous tests.'   )
     
      TRGCDE = 599
      CTRCDE = 5
      OBSCDE = 399

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()

C
C --- Case: ------------------------------------------------------
C               
               TITLE = 'ZZSPKFZT: Target = #; Observer = #; Ref = # '
     .         //      'Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                      OBSCDE, XSTATE, XLT                  ) 
               CALL CHCKXC ( .FALSE., ' ', OK )
   

               CALL ZZSPKFZT ( F_ZZSPKGET,     ET,     REF, 
     .                         ABCORR(CORIDX), OBSCDE, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C

 

 
C*****************************************************************
C*                                                               *
C*                                                               *
C*    ZZSPKFZO tests                                             *
C*                                                               *
C*                                                               *
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad aberration correction.'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFZO ( TRGCDE,  ET,         REF, 
     .                'L+S',   F_ZZSPKGET, STATE, LT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad frame name'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J200'

      ET  = JYEAR()
      
      CALL ZZSPKFZO ( TRGCDE,  ET,         REF, 
     .                'Lt+S',  F_ZZSPKGET, STATE, LT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target data.'  )

      TRGCDE = 777
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFZO ( TRGCDE,  ET,         REF, 
     .                'Lt+S',  F_ZZSPKGET, STATE, LT )

      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no observer data.'  )

      TRGCDE = 5
      CTRCDE = 3
      OBSCDE = 444

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFZO ( TRGCDE,  ET,         REF, 
     .                'Lt+S',  F_ZZSPKGET, STATE, LT )

      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no observer center data.'  )

      TRGCDE = 5
      CTRCDE = 455
      OBSCDE = 4

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFZO ( TRGCDE,  ET,         REF, 
     .                'Lt+S',  F_ZZSPKGET, STATE, LT )

      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C
C     ZZSPKFZO normal cases:
C
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFZO for every aberration correction. '
     .//           'Use different frames. Target = Earth. Center = '
     .//           'Earth-Moon barycenter. Observer = Mars.'        )
 
      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 499

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()

C
C --- Case: ------------------------------------------------------
C               
               TITLE = 'ZZSPKFZO: Target = #; Observer = #; Ref = # '
     .         //      'Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )
  
   
               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                      OBSCDE, XSTATE, XLT                  ) 
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL ZZSPKFZO ( TRGCDE,         ET,         REF, 
     .                         ABCORR(CORIDX), F_ZZSPKGET, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFZO: change observer, target, '
     .//           'and center, and repeat previous tests.'   )
     
      TRGCDE = 599
      CTRCDE = 5
      OBSCDE = 399


      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO CORIDX = 1, NCORR

         DO FRMIDX = 1, NFRAME

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()

C
C --- Case: ------------------------------------------------------
C               
               TITLE = 'ZZSPKFZO: Target = #; Observer = #; Ref = # '
     .         //      'Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )
  
   
               CALL SPKEZ ( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                      OBSCDE, XSTATE, XLT                  ) 
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL ZZSPKFZO ( TRGCDE,         ET,         REF, 
     .                         ABCORR(CORIDX), F_ZZSPKGET, STATE, LT )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


 

 

C*****************************************************************
C*                                                               *
C*                                                               *
C*    ZZSPKFAT tests                                             *
C*                                                               *
C*                                                               *
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad aberration correction.'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFAT ( F_ZZSPKGET,   ET,     REF, 
     .                'L+S',        OBSCDE, STATE, LT, DLT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad frame name'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J200'

      ET  = JYEAR()

      CALL ZZSPKFAT ( F_ZZSPKGET,   ET,     REF, 
     .                'LT+S',       OBSCDE, STATE, LT, DLT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: non-inertial frame'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'IAU_EARTH'

      ET  = JYEAR()

      CALL ZZSPKFAT ( F_ZZSPKGET,   ET,     REF, 
     .                'LT+S',       OBSCDE, STATE, LT, DLT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target data.'  )

      TRGCDE = 777
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFAT ( F_ZZSPKGET,   ET,     REF, 
     .                'LT+S',       OBSCDE, STATE, LT, DLT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target center data.'  )

      TRGCDE = 7
      CTRCDE = 777
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFAT ( F_ZZSPKGET,   ET,     REF, 
     .                'LT+S',       OBSCDE, STATE, LT, DLT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no observer data.'  )

      TRGCDE = 5
      CTRCDE = 3
      OBSCDE = 444

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFAT ( F_ZZSPKGET,   ET,     REF, 
     .                'LT+S',       OBSCDE, STATE, LT, DLT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C
C     ZZSPKFAT normal cases:
C
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFAT for every aberration correction. '
     .//           'Use different frames. Target = Earth. Center = '
     .//           'Earth-Moon barycenter. Observer = Mars.'        )
     
      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 499

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR
C
C        Use only inertial frames.
C
         DO FRMIDX = 1, 2

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()

C
C --- Case: ------------------------------------------------------
C               
               TITLE = 'ZZSPKFAT: Target = #; Observer = #; Ref = # '
     .         //      'Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               CALL SPKACS( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                      OBSCDE, XSTATE, XLT, XDLT            ) 
               CALL CHCKXC ( .FALSE., ' ', OK )
   

               CALL ZZSPKFAT ( F_ZZSPKGET,     ET,     REF, 
     .                         ABCORR(CORIDX), OBSCDE, STATE, LT, DLT )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )


               CALL CHCKSD ( 'Light time rate', 
     .                                     DLT, '~/', XDLT,  TIGHT, OK )


            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFAT: change observer, target, '
     .//           'and center, and repeat previous tests.'   )
     
      TRGCDE = 599
      CTRCDE = 5
      OBSCDE = 399

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO CORIDX = 1, NCORR
C
C        Use only inertial frames.
C
         DO FRMIDX = 1, 2

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()

C
C --- Case: ------------------------------------------------------
C               
               TITLE = 'ZZSPKFAT: Target = #; Observer = #; Ref = # '
     .         //      'Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )


               CALL SPKACS( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                      OBSCDE, XSTATE, XLT, XDLT            ) 
               CALL CHCKXC ( .FALSE., ' ', OK )
   

               CALL ZZSPKFAT ( F_ZZSPKGET,     ET,     REF, 
     .                         ABCORR(CORIDX), OBSCDE, STATE, LT, DLT )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )


               CALL CHCKSD ( 'Light time rate', 
     .                                     DLT, '~/', XDLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C

 



 
C*****************************************************************
C*                                                               *
C*                                                               *
C*    ZZSPKFAO tests                                             *
C*                                                               *
C*                                                               *
C*****************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad aberration correction.'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFAO ( TRGCDE,  ET,         REF, 
     .                'L+S',   F_ZZSPKGET, STATE, LT, DLT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad frame name'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J200'

      ET  = JYEAR()
      
      CALL ZZSPKFAO ( TRGCDE,  ET,         REF, 
     .                'Lt+S',  F_ZZSPKGET, STATE, LT, DLT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: non-inertial frame.'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'IAU_EARTH'

      ET  = JYEAR()
      
      CALL ZZSPKFAO ( TRGCDE,  ET,         REF, 
     .                'Lt+S',  F_ZZSPKGET, STATE, LT, DLT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target data.'  )

      TRGCDE = 777
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFAO ( TRGCDE,  ET,         REF, 
     .                'Lt+S',  F_ZZSPKGET, STATE, LT, DLT )

      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no observer data.'  )

      TRGCDE = 5
      CTRCDE = 3
      OBSCDE = 444

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFAO ( TRGCDE,  ET,         REF, 
     .                'Lt+S',  F_ZZSPKGET, STATE, LT, DLT )

      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no observer center data.'  )

      TRGCDE = 5
      CTRCDE = 455
      OBSCDE = 4

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL ZZSPKFAO ( TRGCDE,  ET,         REF, 
     .                'Lt+S',  F_ZZSPKGET, STATE, LT, DLT )

      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C
C     ZZSPKFAO normal cases:
C
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFAO for every aberration correction. '
     .//           'Use different frames. Target = Earth. Center = '
     .//           'Earth-Moon barycenter. Observer = Mars.'        )
 
      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 499

      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO CORIDX = 1, NCORR
C
C        Use only inertial frames.
C
         DO FRMIDX = 1, 2

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()

C
C --- Case: ------------------------------------------------------
C               
               TITLE = 'ZZSPKFAO: Target = #; Observer = #; Ref = # '
     .         //      'Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )
  
               CALL SPKACS( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                      OBSCDE, XSTATE, XLT, XDLT            ) 
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL ZZSPKFAO ( TRGCDE,         ET,         REF, 
     .                         ABCORR(CORIDX), F_ZZSPKGET, STATE, 
     .                         LT,             DLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )


               CALL CHCKSD ( 'Light time rate', 
     .                                     DLT, '~/', XDLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFAO: change observer, target, '
     .//           'and center, and repeat previous tests.'   )
     
      TRGCDE = 599
      CTRCDE = 5
      OBSCDE = 399


      CALL F_ZZSPKSET ( OBSCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO CORIDX = 1, NCORR
C
C        Use only inertial frames.
C
         DO FRMIDX = 1, 2

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()

C
C --- Case: ------------------------------------------------------
C               
               TITLE = 'ZZSPKFAO: Target = #; Observer = #; Ref = # '
     .         //      'Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )
  
   
               CALL SPKACS( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                      OBSCDE, XSTATE, XLT, XDLT            ) 
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL ZZSPKFAO ( TRGCDE,         ET,         REF, 
     .                         ABCORR(CORIDX), F_ZZSPKGET, STATE, 
     .                         LT,             DLT                )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT, '~/', XLT,  TIGHT, OK )


               CALL CHCKSD ( 'Light time rate', 
     .                                     DLT, '~/', XDLT,  TIGHT, OK )


            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C


 



C*****************************************************************
C*                                                               *
C*                                                               *
C*    ZZSPKFAP tests                                             *
C*                                                               *
C*                                                               *
C*****************************************************************



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad aberration correction.'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL SPKSSB ( OBSCDE, ET, REF, STOBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CLEARD ( 3, ACCOBS )


      CALL ZZSPKFAP ( F_ZZSPKGET, ET,     REF,  'L+S',
     .                STOBS,      ACCOBS, STATE, LT,   DLT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad frame name'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J200'

      ET  = JYEAR()
      
      CALL SPKSSB ( OBSCDE, ET, 'J2000', STOBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSPKFAP ( F_ZZSPKGET, ET,     REF,  'LT+S',
     .                STOBS,      ACCOBS, STATE, LT,   DLT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: non-inertial frame'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'IAU_EARTH'

      ET  = JYEAR()
      
      CALL SPKSSB ( OBSCDE, ET, 'J2000', STOBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSPKFAP ( F_ZZSPKGET, ET,     REF,  'LT+S',
     .                STOBS,      ACCOBS, STATE, LT,   DLT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target data.'  )

      TRGCDE = 777
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL SPKSSB ( OBSCDE, ET, REF, STOBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSPKFAP ( F_ZZSPKGET,  ET,     REF,  'LT+S',
     .                STOBS,       ACCOBS, STATE, LT,   DLT )
      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target center data.'  )

      TRGCDE = 5
      CTRCDE = 333
      OBSCDE = 3

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL SPKSSB ( 3, ET, REF, STOBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSPKFAP ( F_ZZSPKGET,  ET,     REF,  'LT+S',
     .                STOBS,       ACCOBS, STATE, LT,   DLT )
     
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C     
C     ZZSPKFAP normal cases:
C
C

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFAP for every aberration correction. '
     .//           'Use the J2000 frame. Target = Earth. Center = '
     .//           'Moon. Observer = Jupiter barycenter.' )
     
      TRGCDE = 399
      CTRCDE = 301
      OBSCDE = 5
 
      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )
     
 
      DO CORIDX = 1, NCORR
C
C        Use J2000 and a second inertial frame.
C
         DO FRMIDX = 1, 2

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()
C
C --- Case: ------------------------------------------------------
C
               TITLE = 'ZZSPKFAP: Target = #; Observer = #; '
     .         //      'Center = #; Ref = # Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE ) 

C
C              We need the state of the observer relative to the 
C              SSB at ET.
C           
               CALL SPKSSB ( OBSCDE, ET, REF, STOBS )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Compute observer acceleration relative to the 
C              solar system barycenter.
C
               DO I = 1, 2

                  DELTA = DBLE ( 2*I - 3 )

                  CALL SPKSSB ( OBSCDE, ET+DELTA, REF, OBSSSB(1,I) )

               END DO

               CALL QDERIV ( 3, OBSSSB(4,1), OBSSSB(4,2), 1.D0, ACCOBS )

C
C              Get expected state, light time, and light time rate.
C            
               CALL SPKACS ( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                       OBSCDE, XSTATE, XLT, XDLT            ) 
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL ZZSPKFAP ( F_ZZSPKGET,      ET,      REF,   
     .                         ABCORR(CORIDX),  STOBS,   ACCOBS, 
     .                         STATE,           LT,      DLT    )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT,  '~/', XLT,   TIGHT, OK )

               CALL CHCKSD ( 'Light time rate', 
     .                                     DLT, '~/', XDLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C





C*****************************************************************
C*                                                               *
C*                                                               *
C*    ZZSPKFLT tests                                             *
C*                                                               *
C*                                                               *
C*****************************************************************



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad aberration correction.'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL SPKSSB ( OBSCDE, ET, REF, STOBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSPKFLT ( F_ZZSPKGET,  ET,    REF,  'L+S',
     .                STOBS,       STATE, LT,   DLT    )
      
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: bad frame name'  )

      TRGCDE = 399
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J200'

      ET  = JYEAR()
      
      CALL SPKSSB ( OBSCDE, ET, 'J2000', STOBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSPKFLT ( F_ZZSPKGET,  ET,    REF,  'LT+S',
     .                STOBS,       STATE, LT,   DLT    )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target data.'  )

      TRGCDE = 777
      CTRCDE = 3
      OBSCDE = 4

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL SPKSSB ( OBSCDE, ET, REF, STOBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSPKFLT ( F_ZZSPKGET,  ET,    REF,  'LT+S',
     .                STOBS,       STATE, LT,   DLT    )
      
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error case: no target center data.'  )

      TRGCDE = 5
      CTRCDE = 333
      OBSCDE = 3

      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      REF = 'J2000'

      ET  = JYEAR()

      CALL SPKSSB ( 3, ET, REF, STOBS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSPKFLT ( F_ZZSPKGET,  ET,    REF,  'LT+S',
     .                STOBS,       STATE, LT,   DLT    )
     
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


C
C     
C     ZZSPKFLT normal cases:
C
C

 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZSPKFLT for every aberration correction. '
     .//           'Use the J2000 frame. Target = Earth. Center = '
     .//           'Moon. Observer = Jupiter barycenter.' )
     
      TRGCDE = 399
      CTRCDE = 301
      OBSCDE = 5
 
      CALL F_ZZSPKSET ( TRGCDE, CTRCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )
     
 
      DO CORIDX = 1, NCORR
C
C        Use J2000 and a second inertial frame.
C
         DO FRMIDX = 1, 2

            REF = FRAMES(FRMIDX)

            DO TIMIDX = 1, NTIMES

               ET = TIMIDX * JYEAR()
C
C --- Case: ------------------------------------------------------
C
               TITLE = 'ZZSPKFLT: Target = #; Observer = #; '
     .         //      'Center = #; Ref = # Abcorr = #; ET = #.'

               CALL REPMI ( TITLE, '#', TRGCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', OBSCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMI ( TITLE, '#', CTRCDE, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', REF,            TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMC ( TITLE, '#', ABCORR(CORIDX), TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL REPMD ( TITLE, '#', ET, 14, TITLE )
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL TCASE ( TITLE ) 

C
C              We need the state of the observer relative to the 
C              SSB at ET.
C           
               CALL SPKSSB ( OBSCDE, ET, REF, STOBS )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Get expected state, light time, and light time rate.
C            
               CALL SPKLTC ( TRGCDE, ET,     REF, ABCORR(CORIDX), 
     .                       STOBS,  XSTATE, XLT, XDLT            ) 
               CALL CHCKXC ( .FALSE., ' ', OK )


               CALL ZZSPKFLT ( F_ZZSPKGET,     ET,    REF, 
     .                         ABCORR(CORIDX), STOBS, STATE, LT, DLT )
               CALL CHCKXC ( .FALSE., ' ', OK )
 

               CALL CHCKAD ( 'Position', STATE,     '~~/', 
     .                                   XSTATE,    3,     TIGHT, OK )

               CALL CHCKAD ( 'Velocity', STATE(4),  '~~/', 
     .                                   XSTATE(4), 3,     TIGHT, OK )

               CALL CHCKSD ( 'Light time', LT,  '~/', XLT,   TIGHT, OK )

               CALL CHCKSD ( 'Light time rate', 
     .                                     DLT, '~/', XDLT,  TIGHT, OK )

            END DO
C
C           End of time loop.
C
         END DO
C
C        End of frame loop.
C
      END DO
C
C     End of aberration correction loop.
C



C*****************************************************************
C*                                                               *
C*                                                               *
C*    SPKACS tests                                             *
C*                                                               *
C*                                                               *
C*****************************************************************

 
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Make sure that SPKEZ returns the same thing '
     .//           'as SPKACS when an inertial frame is the '
     .//           'requested output frame. Converged Newtonian '
     .//           'plus stellar aberration. Reception case.' )
 
      CALL SPKEZ ( 301001, ET, 'J2000', 'CN+S', 399001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKACS ( 301001, ET, 'J2000', 'CN+S', 399001,
     .              ESTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 6, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Make sure that SPKEZ returns the same thing '
     .//           'as SPKACS when an inertial frame is the '
     .//           'requested output frame. Converged Newtonian '
     .//           'plus stellar aberration. Transmission case.' )

      CALL SPKEZ ( 301001, ET, 'J2000', 'XCN+S', 399001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKACS ( 301001, ET, 'J2000', 'XCN+S', 399001,
     .              ESTATE, ELT, DLT )

      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 6, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )


 
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. Frame center is not '
     .//           'target or observer. CN+S correction. '
     .//           'Reception case.'                          )
 
 
      CALL SPKEZ ( 401001, ET, 'IAU_EARTH', 'CN+S', 301001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      CALL SPKACS ( 401001, ET, 'J2000', 'CN+S', 301001,
     .              ISTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKSSB( 301001, ET, 'J2000',         STOBS )
      CALL SPKLTC( 399,    ET, 'J2000', 'CN+S', STOBS, 
     .             TSTATE, CLT, DLT     )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM( 'J2000', 'IAU_EARTH', ET-CLT,       XFORM       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the derivative block to account for the rate of change
C     of light time.
C
      DO J = 4, 6

         DO I = 1, 3
            XFORM(J,I) = (1.D0 - DLT)*XFORM(J,I)
         END DO

      END DO

      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, VTIGHT, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       VTIGHT, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. Frame center is not '
     .//           'target or observer. CN+S correction. '
     .//           'Transmission case.'                         )
 
 
      CALL SPKEZ ( 401001, ET, 'IAU_EARTH', 'XCN+S', 301001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      CALL SPKACS ( 401001, ET, 'J2000', 'XCN+S', 301001,
     .              ISTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKSSB( 301001, ET, 'J2000',          STOBS )
      CALL SPKLTC( 399,    ET, 'J2000', 'XCN+S', STOBS, 
     .             TSTATE, CLT, DLT                    )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SXFORM( 'J2000', 'IAU_EARTH', ET+CLT,        XFORM       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the derivative block to account for the rate of change
C     of light time.
C
      DO J = 4, 6

         DO I = 1, 3
            XFORM(J,I) = (1.D0 + DLT)*XFORM(J,I)
         END DO

      END DO


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, VTIGHT, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       VTIGHT, OK )




C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. Frame center is '
     .//           'target. CN+S correction. Reception case.'  )
 
 
      CALL SPKEZ ( 399, ET, 'IAU_EARTH', 'CN+S', 301001, STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      CALL SPKACS ( 399, ET, 'J2000', 'CN+S', 301001,
     .              ISTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SXFORM( 'J2000', 'IAU_EARTH', ET-ELT,       XFORM       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the derivative block to account for the rate of change
C     of light time.
C
      DO J = 4, 6

         DO I = 1, 3
            XFORM(J,I) = (1.D0 - DLT)*XFORM(J,I)
         END DO

      END DO


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, VTIGHT, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       VTIGHT, OK )




C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. Frame center is '
     .//           'target. CN+S correction. Transmission case.'  )
 
      CALL SPKEZ ( 399, ET, 'IAU_EARTH', 'XCN+S', 301001, STATE, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      CALL SPKACS ( 399, ET, 'J2000', 'XCN+S', 301001,
     .              ISTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SXFORM( 'J2000', 'IAU_EARTH', ET+ELT,        XFORM       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the derivative block to account for the rate of change
C     of light time.
C
      DO J = 4, 6

         DO I = 1, 3
            XFORM(J,I) = (1.D0 + DLT)*XFORM(J,I)
         END DO

      END DO


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, VTIGHT, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       VTIGHT, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. Frame center is '
     .//           'observer. CN+S correction. Reception case.' )
 
 
      CALL SPKEZ ( 301001, ET, 'IAU_EARTH', 'CN+S', 399, STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKACS( 301001, ET, 'J2000', 'CN+S', 399, 
     .             ISTATE, ELT, DLT       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM( 'J2000', 'IAU_EARTH', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, VTIGHT, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       VTIGHT, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. Frame center is '
     .//           'observer. CN+S correction. Transmission case.' )
 
      CALL SPKEZ ( 301001, ET, 'IAU_EARTH', 'XCN+S', 399, STATE, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKACS( 301001, ET, 'J2000', 'XCN+S', 399, 
     .             ISTATE, ELT, DLT       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM( 'J2000', 'IAU_EARTH', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, VTIGHT, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       VTIGHT, OK )

      CALL SPKUEF (  HANDLE        )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KILFIL ( 'spkfun.bsp' )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
  
      CALL T_SUCCESS ( OK )
      RETURN
      END


C*****************************************************************
C*                                                               *
C*                                                               *
C*    Utility Routines                                           *
C*                                                               *
C*                                                               *
C*****************************************************************


C
C     Subroutine package for referencing SPK ephemerides
C     via external calls:
C
      SUBROUTINE F_ZZEXTSPK ( TRGCDE, ET, REF, INCTR, OUTCTR, STATE )
      IMPLICIT NONE

      INTEGER               TRGCDE
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      INTEGER               INCTR
      INTEGER               OUTCTR
      DOUBLE PRECISION      STATE  ( 6 )

C
C     Local parameters
C

C
C     Local variables
C
      DOUBLE PRECISION      LT 

      INTEGER               SVTCDE
      INTEGER               SVCCDE

C
C     Saved variables
C
      SAVE                  SVCCDE
      SAVE                  SVTCDE


      RETURN

C
C     Save target and center ID codes.
C
      ENTRY F_ZZSPKSET ( TRGCDE, INCTR )

      SVTCDE = TRGCDE
      SVCCDE = INCTR

      RETURN

C
C     Look up state of target relative to center.
C
      ENTRY F_ZZSPKGET ( ET, REF, OUTCTR, STATE )
      
      CALL CHKIN ( 'F_ZZSPKGET' )

      OUTCTR = SVCCDE

      CALL SPKGEO ( SVTCDE, ET, REF, OUTCTR, STATE, LT )

      CALL CHKOUT ( 'F_ZZSPKGET' )
      RETURN

      END





