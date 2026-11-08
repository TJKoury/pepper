C$Procedure F_EK02 ( EK tests, subset 2 )
 
      SUBROUTINE F_EK02 ( OK )
 
C$ Abstract
C
C     Exercise the SPICELIB EK routines, subset 2.
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
  
      INCLUDE 'ekattdsc.inc'
      INCLUDE 'ekbool.inc'
      INCLUDE 'ekcnamsz.inc'
      INCLUDE 'ekglimit.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ektnamsz.inc'
      INCLUDE 'ektype.inc'
      INCLUDE 'errhnd.inc'

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
C     This routine tests the following subset of the EK routines:
C   
C        EKAPPR
C        EKACEC
C        EKACED
C        EKACEI
C        EKBSEG
C        EKDELR
C        EKINSR
C        EKRCEC
C        EKRCED
C        EKRCEI
C        EKUCEC
C        EKUCED
C        EKUCEI
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
C-    TSPICE Version 1.0.1, 20-FEB-2009 (NJB)
C
C        Corrected minor header typo. Re-ordered header sections.
C
C-    TSPICE Version 1.0.0, 14-SEP-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters
C
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

C
C     Local Variables
C
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

      
C
C     Open the test family.
C
      CALL TOPEN ( 'F_EK02' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create a new EK using the record-oriented ' //
     .             'writing routines.'                          )

C
C     Load a leapseconds kernel for UTC/ET conversion.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Open a new EK file. For simplicity, we will not reserve any space
C     for the comment area, so the number of reserved comment
C     characters is zero. The constant IFNAME is the internal file
C     name.
C
      IF ( EXISTS(EK1) ) THEN
         CALL DELFIL ( EK1 )
      END IF

C
C     Create an EK.
C
      CALL EKOPN  ( EK1, IFNAME, NRESVC, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set up the table and column names and declarations for the first
C     segment.  We'll index all of the scalar columns.
C 
      CNAMES(1) = 'INT_COL_1'
      CDECLS(1) = 'DATATYPE = INTEGER, '            //
     .            'INDEXED  = TRUE, '               //
     .            'NULLS_OK = TRUE'

      CNAMES(2) = 'DP_COL_1'
      CDECLS(2) = 'DATATYPE = DOUBLE PRECISION, '   //
     .            'INDEXED  = TRUE, '               //
     .            'NULLS_OK = TRUE'

      CNAMES(3) = 'CHR_COL_1'
      CDECLS(3) = 'DATATYPE = CHARACTER*(*), '      //
     .            'INDEXED  = TRUE, '               //
     .            'NULLS_OK = TRUE'

      CNAMES(4) = 'INT_COL_2'
      CDECLS(4) = 'DATATYPE = INTEGER, '            //
     .            'SIZE     = VARIABLE, '           //
     .            'NULLS_OK = TRUE'

      CNAMES(5) = 'DP_COL_2'
      CDECLS(5) = 'DATATYPE = DOUBLE PRECISION, '   //
     .            'SIZE     = VARIABLE, '           //
     .            'NULLS_OK = TRUE'

      CNAMES(6) = 'CHR_COL_2'
      CDECLS(6) = 'DATATYPE = CHARACTER*(80), '     //
     .            'SIZE     = VARIABLE, '           //
     .            'NULLS_OK = TRUE'


C
C     Start the segment.
C
      CALL EKBSEG ( HANDLE, TABLE, NCOLS, CNAMES, CDECLS, SEGNO )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load values into the columns.
C
      DO I = 0, NROWS-1
C
C        Append an empty record to the segment.
C
         CALL EKAPPR ( HANDLE, SEGNO, RECNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Add an integer column entry.
C
         IVALS(1) = I
         ISNULL   = I .EQ. 1

         CALL EKACEI ( HANDLE, SEGNO, RECNO, CNAMES(1),
     .                 1,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Add a d.p. column entry.
C
         DVALS(1) = I
         ISNULL   = I .EQ. 1

         CALL EKACED ( HANDLE, SEGNO, RECNO, CNAMES(2),
     .                 1,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )
         

C
C        Add a character column entry.
C
         CALL INTSTR ( I, CVALS(1) )
         ISNULL   = I .EQ. 1

         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(3),
     .                 1,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Array-valued columns follow.
C
         IVALS(1) = 10 * I
         IVALS(2) = 10 * I  +  1
         ISNULL   = I .EQ. 1

         CALL EKACEI ( HANDLE, SEGNO, RECNO, CNAMES(4),
     .                 2,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         DVALS(1) = 10 * I
         DVALS(2) = 10 * I  +  1
         DVALS(3) = 10 * I  +  1
         ISNULL   = I .EQ. 1

         CALL EKACED ( HANDLE, SEGNO, RECNO, CNAMES(5),
     .                 3,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         CALL INTSTR ( 10 * I,       CVALS(1) )
         CALL INTSTR ( 10 * I  +  1, CVALS(2) )
         CALL INTSTR ( 10 * I  +  2, CVALS(3) )
         CALL INTSTR ( 10 * I  +  3, CVALS(4) )
         ISNULL   = I .EQ. 1

         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(6),
     .                 4,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     End the file.
C     
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Update the EK: knock out the odd-numbered ' //
     .             'records.'                                    )

C
C     Open the file for write access.
C
      CALL EKOPW ( EK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Knock out all of the records containing even numbers.
C
      SEGNO = 1

      DO RECNO = 1,  NROWS/2
         
         CALL EKDELR ( HANDLE, SEGNO, RECNO )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
      END DO

   
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Replace the missing records with records ' //
     .             'containing the negatives of the original ' //
     .             'values.'                                   )


      DO I = 0, NROWS-1, 2

         RECNO = I+1         
C
C        Insert a record at index RECNO.
C
         CALL EKINSR ( HANDLE, SEGNO, RECNO )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Add a scalar integer column entry.
C
         IVALS(1) = -I
         ISNULL   = I .EQ. 1

         CALL EKACEI ( HANDLE, SEGNO, RECNO, CNAMES(1),
     .                 1,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Add a d.p. column entry.
C
         DVALS(1) = -I
         ISNULL   = I .EQ. 1

         CALL EKACED ( HANDLE, SEGNO, RECNO, CNAMES(2),
     .                 1,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )
         

C
C        Add a character column entry.
C
         CALL INTSTR ( -I, CVALS(1) )
         ISNULL   = I .EQ. 1

         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(3),
     .                 1,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Array-valued columns follow.
C
         IVALS(1) = - ( 10 * I )
         IVALS(2) = - ( 10 * I  +  1 )
         ISNULL   = I .EQ. 1

         CALL EKACEI ( HANDLE, SEGNO, RECNO, CNAMES(4),
     .                 2,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         DVALS(1) = - ( 10 * I )
         DVALS(2) = - ( 10 * I  +  1 )
         DVALS(3) = - ( 10 * I  +  1 )
         ISNULL   = I .EQ. 1

         CALL EKACED ( HANDLE, SEGNO, RECNO, CNAMES(5),
     .                 3,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         CALL INTSTR ( - ( 10 * I ),       CVALS(1) )
         CALL INTSTR ( - ( 10 * I  +  1 ), CVALS(2) )
         CALL INTSTR ( - ( 10 * I  +  2 ), CVALS(3) )
         CALL INTSTR ( - ( 10 * I  +  3 ), CVALS(4) )
         ISNULL   = I .EQ. 1

         CALL EKACEC ( HANDLE, SEGNO, RECNO, CNAMES(6),
     .                 4,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Negate the values in the even-numbered records ' //
     .             'using the update routines.'                      )


      DO I = 1, NROWS-1, 2

         RECNO = I+1         

C
C        Update the scalar integer column entry.
C
         IVALS(1) = -I
         ISNULL   = I .EQ. 1

         CALL EKUCEI ( HANDLE, SEGNO, RECNO, CNAMES(1),
     .                 1,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Update the d.p. column entry.
C
         DVALS(1) = -I
         ISNULL   = I .EQ. 1

         CALL EKUCED ( HANDLE, SEGNO, RECNO, CNAMES(2),
     .                 1,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )
         

C
C        Update the character column entry.
C
         CALL INTSTR ( -I, CVALS(1) )
         ISNULL   = I .EQ. 1

         CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(3),
     .                 1,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Array-valued columns follow.
C
         IVALS(1) = - ( 10 * I )
         IVALS(2) = - ( 10 * I  +  1 )
         ISNULL   = I .EQ. 1

         CALL EKUCEI ( HANDLE, SEGNO, RECNO, CNAMES(4),
     .                 2,      IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         DVALS(1) = - ( 10 * I )
         DVALS(2) = - ( 10 * I  +  1 )
         DVALS(3) = - ( 10 * I  +  1 )
         ISNULL   = I .EQ. 1

         CALL EKUCED ( HANDLE, SEGNO, RECNO, CNAMES(5),
     .                 3,      DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )



         CALL INTSTR ( - ( 10 * I ),       CVALS(1) )
         CALL INTSTR ( - ( 10 * I  +  1 ), CVALS(2) )
         CALL INTSTR ( - ( 10 * I  +  2 ), CVALS(3) )
         CALL INTSTR ( - ( 10 * I  +  3 ), CVALS(4) )
         ISNULL   = I .EQ. 1

         CALL EKUCEC ( HANDLE, SEGNO, RECNO, CNAMES(6),
     .                 4,      CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

C
C     End the file.
C     
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
C
C     Open the file for read access; check the values we've written. 
C

      CALL TCASE ( 'Opening the file for read access; checking ' //
     .             'values using the EK low-level readers.'      )


      CALL EKOPR ( EK1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      DO I = 0, NROWS-1

C
C --- Case: ------------------------------------------------------
C
         RECNO = I+1

         MSG = 'Checking row number #.'
         CALL REPMI ( MSG, '#', RECNO, MSG )
         
         CALL TCASE ( MSG )
         
         XIVALS(1) = -I
         XNULL     =  I .EQ. 1

C
C        Check the scalar integer column entry.
C
         CALL EKRCEI ( HANDLE, SEGNO, RECNO, CNAMES(1),
     .                 NVALS,  IVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'Null flag', ISNULL, XNULL, OK )
         CALL CHCKSI ( 'NVALS', NVALS, '=', 1, 0,  OK )


         IF ( .NOT. ISNULL ) THEN

            LABEL = 'Column'
            CALL SUFFIX ( CNAMES(1), 1, LABEL )

            CALL CHCKSI ( LABEL, IVALS(1), '=', XIVALS(1), 0,  OK )

         END IF


C
C        Check the d.p. column entry.
C
         XDVALS(1) = -I
         XNULL     = I .EQ. 1

         CALL EKRCED ( HANDLE, SEGNO, RECNO, CNAMES(2),
     .                 NVALS,  DVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )
         
         CALL CHCKSL ( 'Null flag', ISNULL, XNULL, OK )
         CALL CHCKSI ( 'NVALS', NVALS, '=', 1, 0,  OK )


         IF ( .NOT. ISNULL ) THEN

            LABEL = 'Column'
            CALL SUFFIX ( CNAMES(2), 1, LABEL )

            CALL CHCKSD ( LABEL, DVALS(1), '=', XDVALS(1), 0.D0,  OK )

         END IF

C
C        Check the character column entry.
C
         CALL INTSTR ( -I, XCVALS(1) )
         XNULL    = I .EQ. 1

         CALL EKRCEC ( HANDLE, SEGNO, RECNO, CNAMES(3),
     .                 NVALS,  CVALS, ISNULL            )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'Null flag', ISNULL, XNULL, OK )
         CALL CHCKSI ( 'NVALS', NVALS, '=', 1, 0,  OK )


         IF ( .NOT. ISNULL ) THEN

            LABEL = 'Column'
            CALL SUFFIX ( CNAMES(3), 1, LABEL )

            CALL CHCKSC ( LABEL, CVALS(1), '=', XCVALS(1), OK )

         END IF




C
C        Array-valued columns follow.
C
         XIVALS(1) = - ( 10 * I )
         XIVALS(2) = - ( 10 * I  +  1 )
         XNULL     = I .EQ. 1

         CALL EKRCEI ( HANDLE, SEGNO, RECNO, CNAMES(4),
     .                 NVALS,  IVALS, ISNULL            )
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

         CALL EKRCED ( HANDLE, SEGNO, RECNO, CNAMES(5),
     .                 NVALS,  DVALS, ISNULL            )
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

         CALL EKRCEC ( HANDLE, SEGNO, RECNO, CNAMES(6),
     .                 NVALS,  CVALS, ISNULL            )
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

C
C     End the file.
C     
      CALL EKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C     Unload the EK.  The TSPICE system will delete the file.
C
      CALL TCASE  ( 'Unload EK from query system.' )

      CALL UNLOAD ( EK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


