!$Procedure F_TPARSE ( Family of tests for TPARSE )

      SUBROUTINE F_TPARSE ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine tests the routine TPARSE. This test is aimed
!     primarily at verifying that the routine has not changed
!     the SP2000 values that it returns from previous editions
!     of the toolkit.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routine TPARSE.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.1, 10-SEP-2020 (JDR)
!
!        Edited the header and test family comments to comply with
!        NAIF standard.
!
!-    TSPICE Version 1.1.0, 30-DEC-1997 (WLT)
!
!        Added sub-case to test conversion of years prior to 1 AD.
!
!-    TSPICE Version 1.0.0, 08-APR-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Test family for TPARSE routine
!
!-&


!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               I

      CHARACTER*(LNSIZE)    TSTRNG ( 23  )
      CHARACTER*(LNSIZE)    ERROR

      DOUBLE PRECISION      EXPDET ( 23  )
      DOUBLE PRECISION      SP2000


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_TPARSE' )


!
!     *****************************************************************
!
!     Normal cases: TPARSE
!
!     *****************************************************************
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Check to make a standard list of strings map ' &
      //           'to the expected ET''s ' )

      CALL TPARCH ( 'NO' )

      TSTRNG (  1  ) = '1/9/1986 3:12:59.2'
      EXPDET (  1  ) =  -441103620.8000D0

      TSTRNG (  2  ) = '9 JAN 1986 03:12:59.2'
      EXPDET (  2  ) =  -441103620.8000D0

      TSTRNG (  3  ) = '1 9 1986 3:12:59.2'
      EXPDET (  3  ) =  -441103620.8000D0

      TSTRNG (  4  ) = '9 JAN 1986 03:12:59.2'
      EXPDET (  4  ) =  -441103620.8000D0

      TSTRNG (  5  ) = '2 jan 1991 3:00:12.2'
      EXPDET (  5  ) =  -283942787.8000D0

      TSTRNG (  6  ) = '2 JAN 1991 03:00:12.2'
      EXPDET (  6  ) =  -283942787.8000D0

      TSTRNG (  7  ) = '1991 MAR 10 12:00:00'
      EXPDET (  7  ) =  -278121600.0000D0

      TSTRNG (  8  ) = '10 MAR 1991 12:00:00'
      EXPDET (  8  ) =  -278121600.0000D0

      TSTRNG (  9  ) = '29 February 1975 3:00'
      EXPDET (  9  ) =  -783853200.0000D0

      TSTRNG ( 10  ) = '1 MAR 1975 03:00:00'
      EXPDET ( 10  ) =  -783853200.0000D0

      TSTRNG ( 11  ) = '2010 October 29 3:58'
      EXPDET ( 11  ) =   341596680.0000D0

      TSTRNG ( 12  ) = '29 OCT 2010 03:58:00'
      EXPDET ( 12  ) =   341596680.0000D0

      TSTRNG ( 13  ) = 'dec 31 86 12'
      EXPDET ( 13  ) =  -410313600.0000D0

      TSTRNG ( 14  ) = '31 DEC 1986 12:00:00'
      EXPDET ( 14  ) =  -410313600.0000D0

      TSTRNG ( 15  ) = '86-365 // 12:00'
      EXPDET ( 15  ) =  -410313600.0000D0

      TSTRNG ( 16  ) = '31 DEC 1986 12:00:00'
      EXPDET ( 16  ) =  -410313600.0000D0

      TSTRNG ( 17  ) = 'JD 2451545.'
      EXPDET ( 17  ) =           0.0000D0

      TSTRNG ( 18  ) = '1 JAN 2000 12:00:00'
      EXPDET ( 18  ) =           0.0000D0

      TSTRNG ( 19  ) = 'jd 2451545.'
      EXPDET ( 19  ) =           0.0000D0

      TSTRNG ( 20  ) = '1 JAN 2000 12:00:00'
      EXPDET ( 20  ) =           0.0000D0

      TSTRNG ( 21  ) = 'JD2451545.'
      EXPDET ( 21  ) =           0.0000D0

      TSTRNG ( 22  ) = '1 JAN 2000 12:00:00'
      EXPDET ( 22  ) =           0.0000D0

      TSTRNG ( 23  ) = '321 B.C. MAR 15 12:00:00'
      EXPDET ( 23  ) = -86400.0D0 * ( 847362.0D0  - 74.0D0 )

      ERROR = ' '

      DO I = 1, 23

         CALL TSTMSG ( '#', 'Test subcase #.' )
         CALL TSTMSI (  I )

         CALL TPARSE ( TSTRNG(I), SP2000, ERROR )
         CALL CHCKXC ( .FALSE., ' ',               OK )
         CALL CHCKSC ( 'ERROR',  ERROR, '=', ' ',  OK )
         CALL CHCKSD ( 'SP2000', SP2000, '=', EXPDET(I), 0.0D0, OK )

      END DO

      CALL T_SUCCESS ( OK )
      RETURN
      END
