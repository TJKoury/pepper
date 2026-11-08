!$Procedure F_EK02 ( EK tests, subset 2 )

      SUBROUTINE F_EK02 ( OK )

! $ Abstract
!
!     Exercise the SPICELIB EK routines, subset 2.
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

      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'errhnd.inc'

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
!     This routine tests the following subset of the EK routines:
!
!        EKAPPR
!        EKACEC
!        EKACED
!        EKACEI
!        EKBSEG
!        EKDELR
!        EKINSR
!        EKRCEC
!        EKRCED
!        EKRCEI
!        EKUCEC
!        EKUCED
!        EKUCEI
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.1, 20-FEB-2009 (NJB)
!
!        Corrected minor header typo. Re-ordered header sections.
!
!-    TSPICE Version 1.0.0, 14-SEP-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         EK1
      PARAMETER           ( EK1    = 'test1.ek' )

      CHARACTER*(*)         IFNAME
      PARAMETER           ( IFNAME = 'Test EK' )

      CHARACTER*(*)         TABLE
      PARAMETER           ( TABLE  = 'TEST_DATA' )


      INTEGER               DECLEN
      PARAMETER           ( DECLEN = 200 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXROW
      PARAMETER           ( MAXROW = 20 )

      INTEGER               CVALSZ
      PARAMETER           ( CVALSZ = MAXSTR )

      INTEGER               CVLEN
      PARAMETER           ( CVLEN  = 80 )

      INTEGER               NCOLS
      PARAMETER           ( NCOLS  = 6 )

      INTEGER               NROWS
      PARAMETER           ( NROWS  = 100 )

      INTEGER               NRESVC
      PARAMETER           ( NRESVC = 0 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL = 10 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

!
!     Local Variables
!
      CHARACTER*(DECLEN)    CDECLS ( MXCLSG )
      CHARACTER*(CNAMSZ)    CNAMES ( MXCLSG )
      CHARACTER*(CVLEN)     CVALS  ( MAXVAL )
      CHARACTER*(CVLEN)     XCVALS ( MAXVAL )
      CHARACTER*(CVLEN)     LABEL
      CHARACTER*(MSGLEN)    MSG

      DOUBLE PRECISION      DVALS  ( MAXVAL )
      DOUBLE PRECISION      XDVALS  ( MAXVAL )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IVALS  ( MAXVAL )
      INTEGER               J
      INTEGER               NVALS
      INTEGER               XIVALS ( MAXVAL )
      INTEGER               RECNO
      INTEGER               SEGNO

      LOGICAL               ISNULL
      LOGICAL               XNULL


!
!     Open the test family.
!
      CALL TOPEN ( 'F_EK02' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create a new EK using the record-oriented ' // &
                   'writing routines.'                          )

!
!     Load a leapseconds kernel for UTC/ET conversion.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Open a new EK file. For simplicity, we will not reserve any space
!     for the comment area, so the number of reserved comment
!     characters is zero. The constant IFNAME is the internal file
!     name.
!
      IF ( EXISTS(EK1) ) THEN
         CALL DELFIL ( EK1 )
      END IF

!
!     Create an EK.
!
      CALL EKOPN  ( EK1, IFNAME, NRESVC, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set up the table and column names and declarations for the first
!     segment.  We'll index all of the scalar columns.
!
      CNAMES(1) = 'INT_COL_1'
      CDECLS(1) = 'DATATYPE = INTEGER, '            // &
                  'INDEXED  = TRUE, '               // &
                  'NULLS_OK = TRUE'

      CNAMES(2) = 'DP_COL_1'
      CDECLS(2) = 'DATATYPE = DOUBLE PRECISION, '   // &
                  'INDEXED  = TRUE, '               // &
                  'NULLS_OK = TRUE'

      CNAMES(3) = 'CHR_COL_1'
      CDECLS(3) = 'DATATYPE = CHARACTER*(*), '      // &
                  'INDEXED  = TRUE, '               // &
                  'NULLS_OK = TRUE'

      CNAMES(4) = 'INT_COL_2'
      CDECLS(4) = 'DATATYPE = INTEGER, '            // &
                  'SIZE     = VARIABLE, '           // &
                  'NULLS_OK = TRUE'

      CNAMES(5) = 'DP_COL_2'
      CDECLS(5) = 'DATATYPE = DOUBLE PRECISION, '   // &
                  'SIZE     = VARIABLE, '           // &
                  'NULLS_OK = TRUE'

      CNAMES(6) = 'CHR_COL_2'
      CDECLS(6) = 'DATATYPE = CHARACTER*(80), '     // &
                  'SIZE     = VARIABLE, '           // &
                  'NULLS_OK = TRUE'


!
!     Start the segment.
!
      CALL EKBSEG ( HANDLE, TABLE, NCOLS, CNAMES, CDECLS, SEGNO )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load values into the columns.
!
      DO I = 0, NROWS-1
!
!        Append an empty record to the segment.
!
         CALL EKAPPR ( HANDLE, SEGNO, RECNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Add an integer column entry.
!
         IVALS(1) = I
         ISNULL   = I .EQ. 1

         CALL EKACEI ( HANDLE, SEGNO, RECNO, CNAMES(1), &
                       1,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Add a d.p. column entry.
!
         DVALS(1) = I
         ISNULL   = I .EQ. 1

         CALL EKACED ( HANDLE, SEGNO, RECNO, CNAMES(2), &
                       1,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Add a character column entry.
!
         CALL INTSTR ( I, CVALS(1) )
         ISNULL   = I .EQ. 1

         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(3), &
                       1,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Array-valued columns follow.
!
         IVALS(1) = 10 * I
         IVALS(2) = 10 * I  +  1
         ISNULL   = I .EQ. 1

         CALL EKACEI ( HANDLE, SEGNO, RECNO, CNAMES(4), &
                       2,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         DVALS(1) = 10 * I
         DVALS(2) = 10 * I  +  1
         DVALS(3) = 10 * I  +  1
         ISNULL   = I .EQ. 1

         CALL EKACED ( HANDLE, SEGNO, RECNO, CNAMES(5), &
                       3,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         CALL INTSTR ( 10 * I,       CVALS(1) )
         CALL INTSTR ( 10 * I  +  1, CVALS(2) )
         CALL INTSTR ( 10 * I  +  2, CVALS(3) )
         CALL INTSTR ( 10 * I  +  3, CVALS(4) )
         ISNULL   = I .EQ. 1

         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(6), &
                       4,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     End the file.
!
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Update the EK: knock out the odd-numbered ' // &
                   'records.'                                    )

!
!     Open the file for write access.
!
      CALL EKOPW ( EK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Knock out all of the records containing even numbers.
!
      SEGNO = 1

      DO RECNO = 1,  NROWS/2

         CALL EKDELR ( HANDLE, SEGNO, RECNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Replace the missing records with records ' // &
                   'containing the negatives of the original ' // &
                   'values.'                                   )


      DO I = 0, NROWS-1, 2

         RECNO = I+1
!
!        Insert a record at index RECNO.
!
         CALL EKINSR ( HANDLE, SEGNO, RECNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Add a scalar integer column entry.
!
         IVALS(1) = -I
         ISNULL   = I .EQ. 1

         CALL EKACEI ( HANDLE, SEGNO, RECNO, CNAMES(1), &
                       1,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Add a d.p. column entry.
!
         DVALS(1) = -I
         ISNULL   = I .EQ. 1

         CALL EKACED ( HANDLE, SEGNO, RECNO, CNAMES(2), &
                       1,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Add a character column entry.
!
         CALL INTSTR ( -I, CVALS(1) )
         ISNULL   = I .EQ. 1

         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(3), &
                       1,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Array-valued columns follow.
!
         IVALS(1) = - ( 10 * I )
         IVALS(2) = - ( 10 * I  +  1 )
         ISNULL   = I .EQ. 1

         CALL EKACEI ( HANDLE, SEGNO, RECNO, CNAMES(4), &
                       2,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         DVALS(1) = - ( 10 * I )
         DVALS(2) = - ( 10 * I  +  1 )
         DVALS(3) = - ( 10 * I  +  1 )
         ISNULL   = I .EQ. 1

         CALL EKACED ( HANDLE, SEGNO, RECNO, CNAMES(5), &
                       3,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         CALL INTSTR ( - ( 10 * I ),       CVALS(1) )
         CALL INTSTR ( - ( 10 * I  +  1 ), CVALS(2) )
         CALL INTSTR ( - ( 10 * I  +  2 ), CVALS(3) )
         CALL INTSTR ( - ( 10 * I  +  3 ), CVALS(4) )
         ISNULL   = I .EQ. 1

         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(6), &
                       4,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Negate the values in the even-numbered records ' // &
                   'using the update routines.'                      )


      DO I = 1, NROWS-1, 2

         RECNO = I+1

!
!        Update the scalar integer column entry.
!
         IVALS(1) = -I
         ISNULL   = I .EQ. 1

         CALL EKUCEI ( HANDLE, SEGNO, RECNO, CNAMES(1), &
                       1,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Update the d.p. column entry.
!
         DVALS(1) = -I
         ISNULL   = I .EQ. 1

         CALL EKUCED ( HANDLE, SEGNO, RECNO, CNAMES(2), &
                       1,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )


!
!        Update the character column entry.
!
         CALL INTSTR ( -I, CVALS(1) )
         ISNULL   = I .EQ. 1

         CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(3), &
                       1,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Array-valued columns follow.
!
         IVALS(1) = - ( 10 * I )
         IVALS(2) = - ( 10 * I  +  1 )
         ISNULL   = I .EQ. 1

         CALL EKUCEI ( HANDLE, SEGNO, RECNO, CNAMES(4), &
                       2,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         DVALS(1) = - ( 10 * I )
         DVALS(2) = - ( 10 * I  +  1 )
         DVALS(3) = - ( 10 * I  +  1 )
         ISNULL   = I .EQ. 1

         CALL EKUCED ( HANDLE, SEGNO, RECNO, CNAMES(5), &
                       3,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         CALL INTSTR ( - ( 10 * I ),       CVALS(1) )
         CALL INTSTR ( - ( 10 * I  +  1 ), CVALS(2) )
         CALL INTSTR ( - ( 10 * I  +  2 ), CVALS(3) )
         CALL INTSTR ( - ( 10 * I  +  3 ), CVALS(4) )
         ISNULL   = I .EQ. 1

         CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(6), &
                       4,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

!
!     End the file.
!
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
!
!     Open the file for read access; check the values we've written.
!

      CALL TCASE ( 'Opening the file for read access; checking ' // &
                   'values using the EK low-level readers.'      )


      CALL EKOPR ( EK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 0, NROWS-1

!
! --- Case: ------------------------------------------------------
!
         RECNO = I+1

         MSG = 'Checking row number #.'
         CALL REPMI ( MSG, '#', RECNO, MSG )

         CALL TCASE ( MSG )

         XIVALS(1) = -I
         XNULL     =  I .EQ. 1

!
!        Check the scalar integer column entry.
!
         CALL EKRCEI ( HANDLE, SEGNO, RECNO, CNAMES(1), &
                       NVALS,  IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'Null flag', ISNULL, XNULL, OK )
         CALL CHCKSI ( 'NVALS', NVALS, '=', 1, 0,  OK )


         IF ( .NOT. ISNULL ) THEN

            LABEL = 'Column'
            CALL SUFFIX ( CNAMES(1), 1, LABEL )

            CALL CHCKSI ( LABEL, IVALS(1), '=', XIVALS(1), 0,  OK )

         END IF


!
!        Check the d.p. column entry.
!
         XDVALS(1) = -I
         XNULL     = I .EQ. 1

         CALL EKRCED ( HANDLE, SEGNO, RECNO, CNAMES(2), &
                       NVALS,  DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'Null flag', ISNULL, XNULL, OK )
         CALL CHCKSI ( 'NVALS', NVALS, '=', 1, 0,  OK )


         IF ( .NOT. ISNULL ) THEN

            LABEL = 'Column'
            CALL SUFFIX ( CNAMES(2), 1, LABEL )

            CALL CHCKSD ( LABEL, DVALS(1), '=', XDVALS(1), 0.D0,  OK )

         END IF

!
!        Check the character column entry.
!
         CALL INTSTR ( -I, XCVALS(1) )
         XNULL    = I .EQ. 1

         CALL EKRCEC ( HANDLE, SEGNO, RECNO, CNAMES(3), &
                       NVALS,  CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'Null flag', ISNULL, XNULL, OK )
         CALL CHCKSI ( 'NVALS', NVALS, '=', 1, 0,  OK )


         IF ( .NOT. ISNULL ) THEN

            LABEL = 'Column'
            CALL SUFFIX ( CNAMES(3), 1, LABEL )

            CALL CHCKSC ( LABEL, CVALS(1), '=', XCVALS(1), OK )

         END IF




!
!        Array-valued columns follow.
!
         XIVALS(1) = - ( 10 * I )
         XIVALS(2) = - ( 10 * I  +  1 )
         XNULL     = I .EQ. 1

         CALL EKRCEI ( HANDLE, SEGNO, RECNO, CNAMES(4), &
                       NVALS,  IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( ISNULL ) THEN

            CALL CHCKSI ( 'NVALS', NVALS, '=', 1, 0,  OK )

         ELSE

            CALL CHCKSI ( 'NVALS', NVALS, '=', 2, 0,  OK )

            LABEL = 'Column'
            CALL SUFFIX ( CNAMES(4), 1, LABEL )

            CALL CHCKAI ( LABEL, IVALS, '=', XIVALS, 2,  OK )

         END IF



         XDVALS(1) = - ( 10 * I )
         XDVALS(2) = - ( 10 * I  +  1 )
         XDVALS(3) = - ( 10 * I  +  1 )
         XNULL     = I .EQ. 1

         CALL EKRCED ( HANDLE, SEGNO, RECNO, CNAMES(5), &
                       NVALS,  DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( ISNULL ) THEN

            CALL CHCKSI ( 'NVALS', NVALS, '=', 1, 0,  OK )

         ELSE

            CALL CHCKSI ( 'NVALS', NVALS, '=', 3, 0,  OK )

            LABEL = 'Column'
            CALL SUFFIX ( CNAMES(5), 1, LABEL )

            CALL CHCKAD ( LABEL, DVALS, '=', XDVALS, 3, 0.D0,  OK )

         END IF



         CALL INTSTR ( - ( 10 * I ),       XCVALS(1) )
         CALL INTSTR ( - ( 10 * I  +  1 ), XCVALS(2) )
         CALL INTSTR ( - ( 10 * I  +  2 ), XCVALS(3) )
         CALL INTSTR ( - ( 10 * I  +  3 ), XCVALS(4) )
         XNULL    = I .EQ. 1

         CALL EKRCEC ( HANDLE, SEGNO, RECNO, CNAMES(6), &
                       NVALS,  CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( ISNULL ) THEN

            CALL CHCKSI ( 'NVALS', NVALS, '=', 1, 0,  OK )

         ELSE

            CALL CHCKSI ( 'NVALS', NVALS, '=', 4, 0,  OK )

            LABEL = 'Column'
            CALL SUFFIX ( CNAMES(6), 1, LABEL )

            DO J = 1, NVALS
               CALL CHCKSC ( LABEL, CVALS(J), '=', XCVALS(J), OK )
            END DO

         END IF

      END DO

!
!     End the file.
!
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!     Unload the EK.  The TSPICE system will delete the file.
!
      CALL TCASE  ( 'Unload EK from query system.' )

      CALL UNLOAD ( EK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


