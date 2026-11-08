!$Procedure ZZGFWSTS ( Private --- GF, sift first window thru second )

      SUBROUTINE ZZGFWSTS ( WNDW1, WNDW2, INCLSN, WNDW3 )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine those intervals of the first window that are
!     properly contained in an interval of the second.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     INTERVALS,  WINDOWS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      WNDW1 ( LBCELL : * )
      DOUBLE PRECISION      WNDW2 ( LBCELL : * )
      CHARACTER*(*)         INCLSN
      DOUBLE PRECISION      WNDW3 ( LBCELL : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ---------------------------------------------------
!     WNDW1      I   Input window 1.
!     WNDW2      I   Input window 2.
!     INCLSN     I   Flag indicating inclusion desired.
!     WNDW3     I/O  Result of sifting WNDW1 through WNDW2.
!
! $ Detailed_Input
!
!     WNDW1      is an initialized SPICELIB window
!
!     WNDW2      is an initialized SPICELIB window
!
!     INCLSN     is a string indicating how intervals of WNDW1 must
!                be contained in WNDW2. Allowed values are: '[]', '(]',
!                '[)', and '()', where a square bracket represents a
!                closed interval and a curved bracket an open interval.
!                Suppose that [a,b] is an interval of WNDW1 and that
!                [c,d] is an interval of WNDW2.  Then the table below
!                shows the tests used to determine the inclusion of
!                [a,b] in the interval from c to d.
!
!                []     ---  [a,b]  is contained in [c,d]
!                (]     ---  [a,b]  is contained in (c,d]
!                [)     ---  [a,b]  is contained in [c,d)
!                ()     ---  [a,b]  is contained in (c,d)
!
!                if INCLSN is not one of these four values, the
!                error SPICE(UNKNOWNINCLUSION) is signaled.
!
!
!
!     WNDW3      is an initialized SPICELIB window, used on input
!                only for the purpose of determining the amount
!                of space declared for use in WNDW3.
!
! $ Detailed_Output
!
!     WNDW3    is a window consisting those of intervals in WNDW1
!              that are wholly contained in some interval of WNDW2.
!
! $ Parameters
!
!     LBCELL     is the SPICELIB cell lower bound.
!
! $ Exceptions
!
!     1) If the window WNDW3 does not have sufficient space to
!        contain the sifting of WNDW1 through WNDW2 the error
!        'SPICE(OUTOFROOM)' is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows the user to specify two closed subsets of the
!     real line and to find the intervals of one that are contained
!     within the intervals of another. The subsets of the real line
!     are assumed to be made up of disjoint unions of closed intervals.
!
! $ Examples
!
!     Suppose that WNDW1 and WNDW2 are described by the tables below.
!
!                    WNDW1                         WNDW2
!                12.3    12.8                  11.7    13.5
!                17.8    20.4                  17.2    18.3
!                21.4    21.7                  18.5    22.6
!                38.2    39.8                  40.1    45.6
!                44.0    59.9
!
!     Then WNDW3 will be given by:
!
!                    WNDW3
!                12.3    12.8
!                21.4    21.7
!
! $ Restrictions
!
!     The set WNDW3 must not overwrite WNDW1 or WNDW2.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     L.S. Elson      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 08-DEC-2010 (EDW)
!
!        Edit to replaced term "schedule" with "window."
!
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT)
!
!-&

! $ Index_Entries
!
! find window intervals contained in an interval of another
!
!-&

!
!     SPICELIB functions
!

      INTEGER               CARDD
      INTEGER               SIZED

      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               INCLEN
      PARAMETER           ( INCLEN = 2 )

!
!     Local Variables
!
      CHARACTER*(INCLEN)    LOCINC

      INTEGER               BEGP1
      INTEGER               BEGP2
      INTEGER               BEGP3
      INTEGER               ENDP1
      INTEGER               ENDP2
      INTEGER               ENDP3
      INTEGER               MAXPTS
      INTEGER               OVFLOW
      INTEGER               SIZE1
      INTEGER               SIZE2

      LOGICAL               CLOSED
      LOGICAL               KEEP
      LOGICAL               LEFT
      LOGICAL               OPEN
      LOGICAL               RIGHT

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFWSTS')

!
!     Store the maximum number of endpoints that can be loaded into
!     WNDW3
!
      MAXPTS      = SIZED ( WNDW3 )
      CALL SSIZED ( MAXPTS, WNDW3 )

!
!     Find the number of endpoints in each of the input windows.
!
      SIZE1     = CARDD( WNDW1 )
      SIZE2     = CARDD( WNDW2 )

!
!     Initialize the place holders for each of the input windows.
!
      BEGP1     =  1
      BEGP2     =  1

      ENDP1     =  2
      ENDP2     =  2

      BEGP3     = -1
      ENDP3     =  0


      CALL CMPRSS ( ' ', 0, INCLSN, LOCINC )

      OPEN   = LOCINC .EQ. '()'
      LEFT   = LOCINC .EQ. '[)'
      RIGHT  = LOCINC .EQ. '(]'
      CLOSED = LOCINC .EQ. '[]'

      IF ( .NOT. (     OPEN &
                  .OR. LEFT &
                  .OR. RIGHT &
                  .OR. CLOSED ) ) THEN

         CALL SETMSG ( 'The value of the inclusion flag must be ' &
         //            'one of the following: ''[]'', ''[)'', ' &
         //            '''(]'', or ''()''.  However the value ' &
         //            'supplied was ''#''. ' )
         CALL ERRCH  ( '#', INCLSN )
         CALL SIGERR ( 'SPICE(UNKNOWNINCLUSION)'  )
         CALL CHKOUT ( 'ZZGFWSTS' )
         RETURN

      END IF

!
!     We haven't had a chance to overflow yet.
!
      OVFLOW    = 0

      DO WHILE (       ( BEGP1 .LT. SIZE1 ) &
                 .AND. ( BEGP2 .LT. SIZE2 ) )
!
!        Using the current interval endpoints determine the overlap of
!        the two intervals.
!
         IF      ( WNDW1(ENDP1) .LT. WNDW2(BEGP2)  ) THEN
!
!           the end of the first interval precedes the beginning of the
!           second
!
            BEGP1    = BEGP1    + 2
            ENDP1    = ENDP1    + 2

         ELSE IF ( WNDW2(ENDP2) .LT. WNDW1(BEGP1) ) THEN
!
!           the end of the second interval precedes the beginning of the
!           first
!
            BEGP2    = BEGP2    + 2
            ENDP2    = ENDP2    + 2

         ELSE
!
!           the intervals intersect.  Is the first contained in the
!           second?
!
            IF      ( CLOSED ) THEN

               KEEP =       ( WNDW1(BEGP1) .GE. WNDW2(BEGP2) ) &
                      .AND. ( WNDW1(ENDP1) .LE. WNDW2(ENDP2) )

            ELSE IF ( OPEN   ) THEN

               KEEP =       ( WNDW1(BEGP1) .GT. WNDW2(BEGP2) ) &
                      .AND. ( WNDW1(ENDP1) .LT. WNDW2(ENDP2) )

            ELSE IF ( LEFT   ) THEN

               KEEP =       ( WNDW1(BEGP1) .GE. WNDW2(BEGP2) ) &
                      .AND. ( WNDW1(ENDP1) .LT. WNDW2(ENDP2) )

            ELSE IF ( RIGHT ) THEN

               KEEP =       ( WNDW1(BEGP1) .GT. WNDW2(BEGP2) ) &
                      .AND. ( WNDW1(ENDP1) .LE. WNDW2(ENDP2) )

            END IF

            IF ( KEEP ) THEN

               BEGP3 = BEGP3    + 2
               ENDP3 = ENDP3    + 2

               IF ( BEGP3 .LT. MAXPTS ) THEN

!
!                 Adequate room is left in WNDW3 to include this
!                 interval
!
                  WNDW3 ( BEGP3 ) = WNDW1(BEGP1)
                  WNDW3 ( ENDP3 ) = WNDW1(ENDP1)

               ELSE

                  OVFLOW = OVFLOW + 2

               END IF

            END IF

!
!           Determine which window pointers to increment
!
            IF      (  WNDW1(ENDP1) .LT. WNDW2(ENDP2)  ) THEN
!
!              The first interval lies before the end of the second
!
               BEGP1    = BEGP1    + 2
               ENDP1    = ENDP1    + 2

            ELSE IF ( WNDW2(ENDP2) .LT. WNDW1(ENDP1) ) THEN
!
!              The second interval lies before the end of the first
!
               BEGP2    = BEGP2    + 2
               ENDP2    = ENDP2    + 2

            ELSE
!
!              The first and second intervals end at the same place
!
               BEGP1    = BEGP1    + 2
               ENDP1    = ENDP1    + 2

               BEGP2    = BEGP2    + 2
               ENDP2    = ENDP2    + 2

            END IF

         END IF

      END DO

      IF ( OVFLOW .GT. 0 ) THEN

         CALL SETMSG ( 'The output window does not have ' &
         //            'sufficient memory to contain the result ' &
         //            'of sifting the two given windows. The ' &
         //            'output window requires space for # ' &
         //            'more values than what has been provided. ')
         CALL ERRINT ( '#', OVFLOW        )
         CALL SIGERR ( 'SPICE(OUTOFROOM)' )

      ELSE

         CALL SCARDD ( ENDP3, WNDW3 )

      END IF

      CALL CHKOUT ( 'ZZGFWSTS' )
      RETURN
      END
