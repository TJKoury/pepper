!$Procedure DAFWDA ( DAF, write data to address )

      SUBROUTINE DAFWDA ( HANDLE, BEGIN, END, DATA )

! $ Abstract
!
!     Write or rewrite the double precision data bounded by two
!     addresses within a DAF.
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
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               BEGIN
      INTEGER               END
      DOUBLE PRECISION      DATA     ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAF.
!     BEGIN,
!     END        I   Initial, final address within file.
!     DATA       I   Data to be stored between BEGIN and END.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF.
!
!     BEGIN,
!     END      are the initial and final addresses of a contiguous
!              set of double precision numbers within a DAF.
!              Presumably, these make up all or part of a
!              particular array.
!
!     DATA     are the double precision data to be stored between
!              the specified addresses within the specified file.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If BEGIN is zero or negative, the error SPICE(DAFNEGADDR)
!         is signaled.
!
!     2)  If the BEGIN > END, the error SPICE(DAFBEGGTEND)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The principal reason that DAFs are so easy to use is that
!     the data in each DAF are considered to be one long contiguous
!     set of double precision numbers. You can store data anywhere
!     within a DAF without knowing (or caring) about the physical
!     records in which they are stored.
!
!     Of course, if you are merely adding arrays to a DAF,
!     you should not use DAFWDA directly, but should use DAFANA
!     (add new array) and its entry points, since these update
!     the appropriate bookkeeping records automatically.
!
! $ Examples
!
!     The following code fragment illustrates the use of DAFWDA
!     to update an imaginary array. The array begins with a directory
!     containing 11 epochs. Each pair of epochs bounds an
!     interval, and each interval is covered by a set of eight
!     osculating elements.
!
!     By accident, the elements were written with the wrong value for
!     the GM of the central body (the last element in each set). Each
!     set must be retrieved, updated,and rewritten.
!
!        CALL DAFUS ( SUM, ND, NI, DC, IC )
!        BEGIN = IC(5)
!
!        DO I = 1, 10
!           OFFSET = BEGIN + 11 + (I - 1) * 8
!
!           CALL DAFRDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
!           ELEMENTS(8) = NEW_GM
!
!           CALL DAFWDA ( HANDLE, OFFSET+1, OFFSET+8, ELEMENTS )
!        END DO
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 27-OCT-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Local variable BUFFER is now initialized and saved.
!
!        Edited the header to comply with NAIF standard. Moved DAF
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     write data to DAF address
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               BSIZE
      PARAMETER           ( BSIZE = 128 )

      INTEGER               BEGR
      INTEGER               BEGW
      INTEGER               ENDR
      INTEGER               ENDW

      INTEGER               RECNO
      DOUBLE PRECISION      BUFFER   ( BSIZE )
      INTEGER               FIRST
      INTEGER               N
      INTEGER               NEXT
      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE                  BUFFER

!
!     Initial values
!
      DATA                  BUFFER / BSIZE * 0.D0 /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DAFWDA' )

!
!     Bad addresses?
!
      IF ( BEGIN .LE. 0 ) THEN
         CALL SETMSG ( 'Negative beginning address: #' )
         CALL ERRINT ( '#', BEGIN )
         CALL SIGERR ( 'SPICE(DAFNEGADDR)' )
         CALL CHKOUT ( 'DAFWDA' )
         RETURN

      ELSE IF ( BEGIN .GT. END ) THEN
         CALL SETMSG ( 'Beginning address (#) greater than ending '   // &
                       'address (#)' )
         CALL ERRINT ( '#', BEGIN )
         CALL ERRINT ( '#', END   )
         CALL SIGERR ( 'SPICE(DAFBEGGTEND)' )
         CALL CHKOUT ( 'DAFWDA' )
         RETURN
      END IF

!
!     Convert raw addresses to record/word representations.
!
      CALL DAFARW ( BEGIN, BEGR, BEGW )
      CALL DAFARW ( END,   ENDR, ENDW )

!
!     The first and last records may have to be read, updated, and
!     rewritten. Any records in between may be written directly.
!
      NEXT = 1

      DO RECNO = BEGR, ENDR

         IF ( RECNO .EQ. BEGR  .OR.  RECNO .EQ. ENDR ) THEN
            CALL DAFRDR ( HANDLE, RECNO, 1, BSIZE, BUFFER, FOUND )

            IF ( .NOT. FOUND ) THEN
               CALL CLEARD ( BSIZE, BUFFER )
            END IF
         END IF

         IF ( BEGR .EQ. ENDR ) THEN
            FIRST = BEGW
            N     = ENDW - BEGW + 1

         ELSE IF ( RECNO .EQ. BEGR ) THEN
            FIRST = BEGW
            N     = BSIZE - BEGW + 1

         ELSE IF ( RECNO .EQ. ENDR ) THEN
            FIRST = 1
            N     = ENDW

         ELSE
            FIRST = 1
            N     = BSIZE
         END IF

         CALL MOVED ( DATA(NEXT), N, BUFFER(FIRST) )
         NEXT = NEXT + N

         CALL DAFWDR ( HANDLE, RECNO, BUFFER )

      END DO

      CALL CHKOUT ( 'DAFWDA' )
      RETURN
      END
