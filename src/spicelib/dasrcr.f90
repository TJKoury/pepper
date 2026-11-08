!$Procedure DASRCR ( DAS, remove comment records )

      SUBROUTINE DASRCR ( HANDLE, N )

! $ Abstract
!
!     Decrease the size of the comment area in a DAS file to reclaim
!     space freed by the removal of a specified number of comment
!     records.
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
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               N

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   A DAS file handle.
!     N          I   Number of comment records to remove.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an existing DAS file opened for
!              comment area modification by DASOPC.
!
!     N        is the number of records to remove from the end of
!              the comment area. of the specified file. If NCOMR
!              is the number of comment records present in the
!              file on input, then on output the number of comment
!              records will be MAX ( 0,  NCOMR - N ).
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input handle is invalid, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If an I/O error occurs during the removal process, the error
!         is signaled by a routine in the call tree of this routine. The
!         DAS file will probably be corrupted in this case.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine is used to reclaim freed space in the comment area
!     of a DAS file subsequent to removal of comments from the file.
!     Any existing directory records and data records will be shifted
!     up by N records.
!
!     This routine updates the file record of the specified DAS file
!     to reflect the addition of records to the file's comment area.
!     Also, the file summary obtainable from DASHFS will be updated to
!     reflect the addition of comment records.
!
!     The disk space occupied by the specified DAS file will not
!     decrease as a result of calling this routine, but the number of
!     records occupied by meaningful data will decrease. The useful
!     records in the file can be copied by DAS routines to create a
!     new, smaller file which contains only the meaningful data.
!
!     This routine may be used only on existing DAS files opened by
!     DASOPC.
!
!     The association of DAS logical addresses and data within the
!     specified file will remain unaffected by use of this routine.
!
!     Normally, SPICELIB applications will not call this routine
!     directly, but will remove comments by calling DASRC.
!
!     This routine has an inverse DASACR, which appends a specified
!     number of records to the end of the comment area.
!
! $ Examples
!
!     C
!     C     Open an existing DAS file for modification of
!     C     the comment area. We'll presume that the file
!     C     contains 20 comment records.
!     C
!           CALL DASOPC ( DAS, HANDLE )
!
!     C
!     C     Remove the last 10 comment records from the file.
!     C
!           CALL DASRCR ( HANDLE, 10  )
!
!     C
!     C     Close the file.
!     C
!           CALL DASCLS ( HANDLE )
!
! $ Restrictions
!
!     1)  The DAS file must have a binary file format native to the host
!         system.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 02-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 05-FEB-2015 (NJB)
!
!        Updated to support integration with the handle
!        manager subsystem.
!
!        Cleaned up use of unnecessary variables and unneeded
!        declarations.
!
!-    SPICELIB Version 1.0.0, 15-NOV-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     remove comment records from a DAS file
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN


!
!     Local parameters
!

!
!     Words per data record, for each data type:
!
      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )

      INTEGER               NWD
      PARAMETER           ( NWD    =  128 )

      INTEGER               NWI
      PARAMETER           ( NWI    =  256 )

!
!     Data type parameters
!
      INTEGER               CHAR
      PARAMETER           ( CHAR   =   1  )

      INTEGER               DP
      PARAMETER           ( DP     =   2  )

!
!     Directory pointer location (forward):
!
      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC =   1  )

!
!     Location of first type descriptor
!
      INTEGER               BEGDSC
      PARAMETER           ( BEGDSC = 9 )


!
!     Local variables
!
      CHARACTER*(NWC)       RECC

      DOUBLE PRECISION      RECD   ( NWD )

      INTEGER               BASE
      INTEGER               DIRREC ( NWI )
      INTEGER               FREE
      INTEGER               I
      INTEGER               LASTLA ( 3 )
      INTEGER               LASTRC ( 3 )
      INTEGER               LASTWD ( 3 )
      INTEGER               LOC
      INTEGER               LREC
      INTEGER               LINDEX
      INTEGER               LWORD
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NREC
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NSHIFT
      INTEGER               POS
      INTEGER               RECI   ( NWI    )
      INTEGER               TYPE
      INTEGER               UNIT


!
!     Saved variables
!

!
!     NEXT and PREV map the DAS data type codes to their
!     successors and predecessors, respectively.
!
      INTEGER               NEXT   ( 3 )
      SAVE                  NEXT

      INTEGER               PREV   ( 3 )
      SAVE                  PREV

!
!     Initial values
!
      DATA                  NEXT   /  2,   3,   1  /
      DATA                  PREV   /  3,   1,   2  /



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DASRCR' )

!
!     Make sure this DAS file is open for writing.  Signal an error if
!     not.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

!
!     Get the logical unit for this DAS file.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DASRCR' )
         RETURN
      END IF

!
!     It's a mistake to use a negative value of N.
!
      IF ( N .LT. 0 ) THEN

         CALL SETMSG ( 'Number of comment records to remove must be ' // &
                       'non-negative.  Actual number requested was #.' )
         CALL ERRINT ( '#',  N                                         )
         CALL SIGERR ( 'SPICE(DASINVALIDCOUNT)'                        )
         CALL CHKOUT ( 'DASRCR'                                        )
         RETURN

      END IF

!
!     Before doing anything to the file, make sure that the DASRWR
!     data buffers do not contain any updated records for this file.
!     All of the record numbers that pertain to this file and remain
!     in the DASRWR buffers will be invalidated after this routine
!     returns.
!
!     DASWBR flushes buffered records to the file.
!
      CALL DASWBR ( HANDLE )

!
!     Grab the file summary for this DAS file.  Find the number of
!     reserved records and the number of the first free record.
!
      CALL DASHFS ( HANDLE, &
                    NRESVR, &
                    NRESVC, &
                    NCOMR, &
                    NCOMC, &
                    FREE, &
                    LASTLA, &
                    LASTRC, &
                    LASTWD )


!
!     Determine the size of the record shift we'll actually perform.
!
      NSHIFT  =  MIN ( N, NCOMR )

!
!     Find the record and word positions LREC and LWORD of the last
!     descriptor in the file.
!
      CALL MAXAI  ( LASTRC,  3,  LREC,  LOC )
      LWORD  =  0

      DO I = 1, 3

         IF (       ( LASTRC(I) .EQ. LREC  ) &
              .AND. ( LASTWD(I) .GT. LWORD )  ) THEN

            LWORD = LASTWD(I)

         END IF

      END DO

!
!     LREC and LWORD are now the record and word index of the last
!     descriptor in the file. If LREC is zero, there are no directories
!     in the file yet. However, even DAS files that don't contain any
!     data have their first directory records zeroed out, and this
!     should remain true after the removal of the comment records.
!
      IF ( LREC .EQ. 0 ) THEN
!
!        Just write the zero-filled record to record number
!
!           NRESVR + NCOMR + 2 - NSHIFT
!
         CALL CLEARI (  NWI,                                 DIRREC )
         CALL DASIOI ( 'WRITE', UNIT, NRESVR+NCOMR+2-NSHIFT, DIRREC )

      ELSE
!
!        There really is stuff to move.  For each directory record,
!        move the record and then all of the records described by that
!        record.  We start at the beginning of the data area and move
!        downwards in the file as we go.
!
         NREC  =  NRESVR + NCOMR + 2

         DO WHILE ( ( NREC .LE. LREC ) .AND. ( NREC .NE. 0 ) )
!
!           Read the current directory record and move it.
!
            CALL DASIOI ( 'READ',   UNIT,  NREC,         DIRREC )
            CALL DASIOI ( 'WRITE',  UNIT,  NREC-NSHIFT,  DIRREC )

!
!           For each descriptor in the current directory, move the
!           cluster of data records it refers to.
!
!           Find the data type, size, and base record number of the
!           first cluster described by the current directory.  Also
!           find the index within the directory of the directory's
!           last descriptor.
!
            TYPE  =  DIRREC( BEGDSC )
            BASE  =  NREC + 1

            IF ( NREC .EQ. LREC ) THEN
               LINDEX = LWORD
            ELSE
               LINDEX = NWI
            END IF

!
!           We'll now traverse the directory in forward order, keeping
!           track of cluster sizes and types as we go.
!
!           POS will be the index of the descriptor of the current
!           cluster.
!
            POS  =  BEGDSC + 1

            DO WHILE ( POS .LE. LINDEX )

               IF ( POS .GT. BEGDSC+1 ) THEN
!
!                 We'll need to determine the type of the current
!                 cluster.  If the descriptor contains a positive
!                 value, the data type of the cluster it refers to is
!                 the successor of the previous type, according to our
!                 ordering of types.
!
                  IF (  DIRREC(POS)  .GT.  0  ) THEN
                     TYPE  =  NEXT( TYPE )
                  ELSE
                     TYPE  =  PREV( TYPE )
                  END IF

!
!                 Update the cluster base record number.
!
                  BASE  =  BASE + ABS ( DIRREC(POS-1) )

               END IF

!
!              BASE and TYPE now are correctly set for the current
!              cluster.  Move the cluster.
!
               DO I  =  BASE,   BASE + ABS( DIRREC(POS) ) - 1

                  IF ( TYPE .EQ. CHAR ) THEN

                     CALL DASIOC ( 'READ',   UNIT,  I,         RECC )
                     CALL DASIOC ( 'WRITE',  UNIT,  I-NSHIFT,  RECC )

                  ELSE IF ( TYPE .EQ. DP ) THEN

                     CALL DASIOD ( 'READ',   UNIT,  I,         RECD )
                     CALL DASIOD ( 'WRITE',  UNIT,  I-NSHIFT,  RECD )

                  ELSE

                     CALL DASIOI ( 'READ',   UNIT,  I,         RECI )
                     CALL DASIOI ( 'WRITE',  UNIT,  I-NSHIFT,  RECI )

                  END IF

               END DO

!
!              The next descriptor to look at is the next one in the
!              current directory.
!
               POS     =  POS + 1

            END DO

!
!           Find the next directory record.
!
            NREC = DIRREC ( FWDLOC )

         END DO

      END IF

!
!     Update the file summary.  The number of comment records and the
!     number of the first free record have been decremented by NSHIFT.
!     The numbers of the records containing the last descriptor of each
!     type have been decremented by NSHIFT only if they were non-zero.
!
!
!     The call to DASUFS will update the file record as well as the
!     file summary.
!
      NCOMR  =  NCOMR - NSHIFT
      FREE   =  FREE  - NSHIFT

      DO I = 1, 3

         IF ( LASTRC(I) .NE. 0 ) THEN
            LASTRC(I)  =  LASTRC(I) - NSHIFT
         END IF

      END DO

      CALL DASUFS ( HANDLE, &
                    NRESVR, &
                    NRESVC, &
                    NCOMR, &
                    NCOMC, &
                    FREE, &
                    LASTLA, &
                    LASTRC, &
                    LASTWD )

      CALL CHKOUT ( 'DASRCR' )
      RETURN
      END
