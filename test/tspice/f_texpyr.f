C$Procedure F_TEXPYR ( Family of tests for TEXPYR )

      SUBROUTINE F_TEXPYR ( OK )

C$ Abstract
C
C     This routine tests the routine TEXPYR and its companion
C     entry point TSETYR.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the SPICELIB routines TEXPYR and TSETYR.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C     F.S. Turner        (JPL)
C
C$ Version
C
C-    TSPICE Version 2.1.0, 07-DEC-2020 (JDR)
C
C        Added test case for non-positive inputs to TSETYR.
C
C        Edited the header and comments to comply with NAIF standard.
C
C-    TSPICE Version 2.0.0, 13-DEC-2001 (FST)
C
C        Updated this routine to restore and exercise the
C        default value of 1969 for TSETYR.
C
C-    TSPICE Version 1.0.0, 26-MAY-1998 (WLT)
C
C        Initial version.
C
C-&


C$ Index_Entries
C
C     Test family for TEXPYR and TSETYR routines
C
C-&


C
C     Local Variables
C
      INTEGER               YEAR
      INTEGER               EXPYR
      INTEGER               I
      INTEGER               J


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_TEXPYR' )


C
C     *****************************************************************
C
C     Normal cases: TEXPYR and TSETYR
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that in the default case two digit '
     .//           'years range are mapped to the interval from '
     .//           '1969 to 2068.')


      DO I = 0, 99

         YEAR = I

         CALL TEXPYR ( YEAR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .LE. 68 ) THEN
            EXPYR = 2000 + I
         ELSE
            EXPYR = 1900 + I
         END IF

         CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      END DO


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that when we set the lower bound '
     .//           'for the century that all two digit years are '
     .//           'expanded appropriately. ' )


      CALL TSETYR ( 2025 )

      DO I = 0, 99

         YEAR = I

         CALL TEXPYR ( YEAR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .LE. 24 ) THEN
            EXPYR = 2100 + I
         ELSE
            EXPYR = 2000 + I
         END IF

         CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      END DO

C
C     Reset the lower bound and re-do the first set of tests.
C
      CALL TSETYR ( 1969 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 0, 99

         YEAR = I

         CALL TEXPYR ( YEAR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( I .LE. 68 ) THEN
            EXPYR = 2000 + I
         ELSE
            EXPYR = 1900 + I
         END IF

         CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that non-positive years provided to '
     .//           'TSETYR do not change the lower bound for the '
     .//           'year expansion.' )

      DO J = -1, 0

         CALL TSETYR ( J )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO I = 0, 99

            YEAR = I

            CALL TEXPYR ( YEAR )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           Lower bound should still be 1969.
C
            IF ( I .LE. 68 ) THEN
               EXPYR = 2000 + I
            ELSE
               EXPYR = 1900 + I
            END IF

            CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

         END DO

      END DO


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Make sure that years outside the inclusive '
     .//           'range from 0 to 99 are not altered by '
     .//           'TEXPYR. ' )

      YEAR  = 1928
      EXPYR = 1928

      CALL TEXPYR ( YEAR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      YEAR  = 100
      EXPYR = 100

      CALL TEXPYR ( YEAR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )

      YEAR  = -1
      EXPYR = -1

      CALL TEXPYR ( YEAR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'YEAR', YEAR, '=', EXPYR, 0, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
