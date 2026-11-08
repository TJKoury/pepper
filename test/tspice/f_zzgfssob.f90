!$Procedure      F_ZZGFSSOB ( Test sub-observer state computation )

      SUBROUTINE F_ZZGFSSOB ( OK )

! $ Abstract
!
!     Test the GF private sub-observer state routine ZZGFSSOB.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzabcorr.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB GF
!     sub-observer state routine ZZGFSSOB.
!
! $ Examples
!
!     None.
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
!-    SPICELIB Version 2.0.0, 12-MAY-2009 (NJB)
!
!        Removed error checks for ID codes that don't
!        map to names, as these are no longer applicable.
!
!-    SPICELIB Version 1.0.0, 19-MAY-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD

!
!     Local parameters
!
!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgfssob.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgfssob.bsp' )


      DOUBLE PRECISION      MEDABS
      PARAMETER           ( MEDABS = 5.D-5 )

      DOUBLE PRECISION      MEDREL
      PARAMETER           ( MEDREL = 1.D-8 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-10 )

!
!     Time delta used for discrete derivatives. The 10-second
!     value used here was found by trial and error: it yields
!     better approximations than does the "standard" 1-second
!     value.
!
      DOUBLE PRECISION      TDELTA
      PARAMETER           ( TDELTA = 1.D1 )



      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               NMETH
      PARAMETER           ( NMETH = 2 )


      INTEGER               NSAMP
      PARAMETER           ( NSAMP = 10 )



!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    METH   ( NMETH )
      CHARACTER*(LNSIZE)    METHOD

      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SPNT   ( 3, 2 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      XPNT   ( 3 )
      DOUBLE PRECISION      XVEL   ( 3 )

      INTEGER               NC
      INTEGER               NM
      INTEGER               NS
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               N
      INTEGER               TRGID
      INTEGER               OBSID

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND

!
!     Save everything.
!
      SAVE

!
!     Initial values
!
      DATA                  CORR / 'NONE', &
                                   'lt',  ' lt+s',  ' cn',  ' cn + s', &
                                   'XLT', 'XLT + S', 'XCN', 'XCN+S'   /


      DATA                  METH / 'Near point: ellipsoid', &
                                   'Intercept:  ellipsoid'  /







!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFSSOB' )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load a PCK.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Actual DE-based ephemerides yield better comparisons,
!     since these ephemerides have less noise than do those
!     produced by TSTSPK.
!
!      CALL FURNSH ( 'de421.bsp' )
!      CALL FURNSH ( 'jup230.bsp' )
!


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction' )

      ABCORR = 'None.'
      METHOD = 'Near point: ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      FIXREF = 'IAU_EARTH'


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSOB ( METHOD, TRGID, ET,    FIXREF, &
                      ABCORR, OBSID, RADII, STATE   )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad computation method' )

      ABCORR = 'None'
      METHOD = 'Near point'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      FIXREF = 'IAU_EARTH'


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSOB ( METHOD, TRGID, ET,    FIXREF, &
                      ABCORR, OBSID, RADII, STATE   )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target orientation data available' )

      ABCORR = 'None'
      METHOD = 'Near point: ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      FIXREF = 'ITRF93'


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSOB ( METHOD, TRGID, ET,    FIXREF, &
                      ABCORR, OBSID, RADII, STATE   )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reference frame not centered on target' )

      ABCORR = 'None'
      METHOD = 'Near point: ellipsoid'
      TARGET = 'Mars'
      OBSRVR = 'MOON'
      FIXREF = 'IAU_EARTH'


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSOB ( METHOD, TRGID, ET,    FIXREF, &
                      ABCORR, OBSID, RADII, STATE   )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target ephemeris data available' )

      ABCORR = 'None'
      METHOD = 'Near point: ellipsoid'
      TARGET = 'GASPRA'
      OBSRVR = 'MOON'
      FIXREF = 'IAU_GASPRA'


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSOB ( METHOD, TRGID, ET,    FIXREF, &
                      ABCORR, OBSID, RADII, STATE   )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No observer ephemeris data available' )

      ABCORR = 'None'
      METHOD = 'Near point: ellipsoid'
      TARGET = 'MOON'
      OBSRVR = 'GASPRA'
      FIXREF = 'IAU_MOON'


      TIMSTR = '2008 MAY 1'

      CALL STR2ET ( TIMSTR, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( TARGET, TRGID, FOUND )
      CALL BODN2C ( OBSRVR, OBSID, FOUND )

      CALL ZZGFSSOB ( METHOD, TRGID, ET,    FIXREF, &
                      ABCORR, OBSID, RADII, STATE   )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************


!
!     We're going to loop over all aberration corrections and
!     computation methods.
!

      DO NC = 1, NCORR

         DO NM = 1, NMETH

!           Loop over the set of sample times.
!
            TIMSTR = '2008 MAY 1'

            CALL STR2ET ( TIMSTR, ET0 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DELTA = 100 * SPD()


            DO NS = 1, NSAMP

!
! --- Case: ------------------------------------------------------
!
               ABCORR = CORR(NC)
               METHOD = METH(NM)
               TARGET = 'EARTH'
               OBSRVR = 'MOON'
               FIXREF = 'IAU_EARTH'

               TITLE = 'sub-# point on #. #; #; #.'

               CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
               CALL REPMC ( TITLE, '#', TARGET, TITLE )
               CALL REPMC ( TITLE, '#', METHOD, TITLE )
               CALL REPMC ( TITLE, '#', ABCORR, TITLE )

               ET = ET0 + (NS-1)*DELTA
               CALL ETCAL ( ET, TIMSTR )
               CALL REPMC ( TITLE, '#', TIMSTR,  TITLE )

               CALL TCASE ( TITLE )

               CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL BODN2C ( TARGET, TRGID, FOUND )
               CALL BODN2C ( OBSRVR, OBSID, FOUND )

               CALL ZZGFSSOB ( METHOD, TRGID, ET,    FIXREF, &
                               ABCORR, OBSID, RADII, STATE   )

               CALL CHCKXC ( .FALSE., ' ', OK )


!
!              Estimate the sub-point velocity numerically.
!
               DO I = 1, 2

                  T = ET + (2*I-3)*TDELTA

                 CALL SUBPNT ( METHOD, TARGET, T,         FIXREF, &
                               ABCORR, OBSRVR, SPNT(1,I), TRGEPC, &
                               SRFVEC                            )
                 CALL CHCKXC ( .FALSE., ' ', OK )

              END DO


              CALL QDERIV ( 3, SPNT(1,1), SPNT(1,2), TDELTA, XVEL )
              CALL CHCKXC ( .FALSE., ' ', OK )

!
!             Check the velocity from ZZGFSSOB. We expect an error of
!             less than 1 cm/s.
!
              CALL CHCKAD ( 'Sub-point vel', STATE(4), '~~', &
                                             XVEL,  3,  MEDABS, OK )


              CALL SUBPNT ( METHOD, TARGET, ET,   FIXREF, &
                            ABCORR, OBSRVR, XPNT, TRGEPC, SRFVEC )

!
!             The tolerance we use depends on the aberration correction.
!
              CALL ZZPRSCOR ( ABCORR, ATTBLK )

              IF ( ATTBLK(CNVIDX) ) THEN

                 TOL = TIGHT

              ELSE IF ( ATTBLK(LTIDX) ) THEN

                 TOL = MEDREL
              ELSE
                 TOL = TIGHT
              END IF

              CALL CHCKAD ( 'Sub-point', STATE, '~~/', &
                                         XPNT,  3,  TOL, OK )

!
! --- Case: ------------------------------------------------------
!
               ABCORR = CORR(NC)
               METHOD = METH(NM)
               TARGET = 'JUPITER'
               OBSRVR = 'IO'
               FIXREF = 'IAU_JUPITER'


               TITLE = 'sub-# point on #. #; #; #.'

               CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
               CALL REPMC ( TITLE, '#', TARGET, TITLE )
               CALL REPMC ( TITLE, '#', METHOD, TITLE )
               CALL REPMC ( TITLE, '#', ABCORR, TITLE )

               ET = ET0 + (NS-1)*DELTA
               CALL ETCAL ( ET, TIMSTR )
               CALL REPMC ( TITLE, '#', TIMSTR,  TITLE )

               CALL TCASE ( TITLE )


               CALL STR2ET ( TIMSTR, ET )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL BODN2C ( TARGET, TRGID, FOUND )
               CALL BODN2C ( OBSRVR, OBSID, FOUND )

               CALL ZZGFSSOB ( METHOD, TRGID, ET,    FIXREF, &
                               ABCORR, OBSID, RADII, STATE   )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Estimate the sub-point velocity numerically.
!
               DO I = 1, 2

                  T = ET + (2*I-3)*TDELTA

                  CALL SUBPNT ( METHOD, TARGET, T,         FIXREF, &
                                ABCORR, OBSRVR, SPNT(1,I), TRGEPC, &
                                SRFVEC                            )
                  CALL CHCKXC ( .FALSE., ' ', OK )

               END DO

               CALL QDERIV ( 3, SPNT(1,1), SPNT(1,2), TDELTA, XVEL )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!             Check the velocity from ZZGFSSOB. We expect an error of
!             less than 1 cm/s.
!
              CALL CHCKAD ( 'Sub-point vel', STATE(4), '~~', &
                                             XVEL,  3,  MEDABS, OK )

!
!             Make sure the position component of STATE matches the
!             result from SUBPNT.
!
              CALL SUBPNT ( METHOD, TARGET, ET,   FIXREF, &
                            ABCORR, OBSRVR, XPNT, TRGEPC, SRFVEC )

!
!             The tolerance we use depends on the aberration correction.
!
              CALL ZZPRSCOR ( ABCORR, ATTBLK )

              IF ( ATTBLK(CNVIDX) ) THEN

                 TOL = TIGHT

              ELSE IF ( ATTBLK(LTIDX) ) THEN

                 TOL = MEDREL
              ELSE
                 TOL = TIGHT
              END IF

              CALL CHCKAD ( 'Sub-point', STATE, '~~/', &
                                         XPNT,  3,  TOL, OK )

            END DO
!
!           End of cases for the current epoch.
!
         END DO
!
!        End of cases for the current method.
!
      END DO
!
!     End of cases for the current aberration correction.
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL T_SUCCESS ( OK )

      RETURN
      END




