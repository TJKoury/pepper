!$Procedure DASACR ( DAS, add comment records )

      SUBROUTINE DASACR ( HANDLE, N )

! $ Abstract
!
!     Increase the size of the comment area in a DAS file to accommodate
!     a specified number of additional comment records.
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
!     N          I   Number of comment records to append to the comment
!                    area of the specified file.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of an existing DAS file opened for
!              comment area modification by DASOPC.
!
!     N        is the number of records to append to the comment
!              area. If NCOMR is the number of comment records
!              present in the file on input, then on output the
!              number of comment records will be NCOMR + N.
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
!     2)  If an I/O error occurs during the addition process, the error
!         is signaled by a routine in the call tree of this routine. The
!         DAS file will probably be corrupted in this case.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     This routine is used to create space in the comment area of a DAS
!     file to allow addition of comments to the file. If there are
!     comment records present in the file at the time this routine is
!     called, the number of comment records specified by the input
!     argument N will be appended to the existing comment records.
!     In any case, any existing directory records and data records will
!     be shifted down by N records.
!
!     This routine updates the file record of the specified DAS file
!     to reflect the addition of records to the file's comment area.
!     Also, the file summary obtainable from DASHFS will be updated to
!     reflect the addition of comment records.
!
!     This routine may be used only on existing DAS files opened by
!     DASOPW.
!
!     The association of DAS logical addresses and data within the
!     specified file will remain unaffected by use of this routine.
!
!     Normally, SPICELIB applications will not call this routine
!     directly, but will add comments by calling DASAC.
!
!     This routine has an inverse DASRCR, which removes a specified
!     number of records from the end of the comment area.
!
! $ Examples
!
!     1)  Make room for 10 comment records in the comment area of a
!         new DAS file.
!
!            C
!            C     Create a new DAS file.
!            C
!                  CALL DASOPW ( DAS, HANDLE )
!
!            C
!            C     Now add 10 comment records to the comment area.
!            C
!                  CALL DASACR ( HANDLE, 10 )
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
!-    SPICELIB Version 1.3.0, 13-AUG-2021 (JDR)
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
!-    SPICELIB Version 1.1.0, 11-OCT-1996 (NJB)
!
!        Bug fix: backward and forward directory record pointers
!        are now updated when directory records are moved.
!
!-    SPICELIB Version 1.0.0, 01-FEB-1993 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     add comment records to a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 11-OCT-1996 (NJB)
!
!        Bug fix: backward and forward directory record pointers
!        are now updated when directory records are moved.
!
!        Because these pointers are not used by the DAS software
!        once a DAS file is segregated, this bug had no effect on
!        DAS files that were created and closed via DASCLS, then
!        commented via the commnt utility.
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
!     Directory pointer locations (backward and forward):
!
      INTEGER               BWDLOC
      PARAMETER           ( BWDLOC =   1  )

      INTEGER               FWDLOC
      PARAMETER           ( FWDLOC =   2  )

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
      INTEGER               NXTTYP
      INTEGER               POS
      INTEGER               PREC
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

      CALL CHKIN ( 'DASACR' )

!
!     Programmer's note: the calls to
!
!        DASIOC
!        DASIOD
!        DASIOI
!
!     for read access are valid only for native format DAS files.
!     If this routine is updated to support writing to non-native
!     DAS files, at least the calls to the numeric I/O routines
!     will need to be replaced. (Consider using ZZDASGRD, ZZDASGRI.)
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
         CALL CHKOUT ( 'DASACR' )
         RETURN
      END IF

!
!     It's a mistake to use a negative value of N.
!
      IF ( N .LT. 0 ) THEN

         CALL SETMSG ( 'Number of comment records to add must be '    // &
                       'non-negative.  Actual number requested was #.' )
         CALL ERRINT ( '#',  N                                         )
         CALL SIGERR ( 'SPICE(DASINVALIDCOUNT)'                        )
         CALL CHKOUT ( 'DASACR'                                        )
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
!     comment records and the number of the first free record.
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
!     LREC and LWORD are now the record and word index of the last word
!     of the last descriptor in the file. If LREC is zero, there are no
!     directories in the file yet. However, even DAS files that don't
!     contain any data have their first directory records zeroed out,
!     and this should remain true after the addition of the comment
!     records.
!
      IF ( LREC .EQ. 0 ) THEN
!
!        Just write the zero-filled record to record number
!
!           NRESVR + NCOMR + N + 2
!
         CALL CLEARI ( NWI,                             DIRREC )
         CALL DASIOI ( 'WRITE', UNIT, NRESVR+NCOMR+N+2, DIRREC )


      ELSE
!
!        There really is stuff to move.  For each directory record,
!        move all of the records described by that directory.  We start
!        with the last directory and work our way toward the beginning
!        of the file.
!
         NREC  =  LREC

         DO WHILE ( NREC .GT. 0 )
!
!           For each descriptor in the current directory, move the
!           cluster of data records it refers to.
!
!           Read the current directory record.
!
            CALL DASIOI ( 'READ', UNIT, NREC, DIRREC )

!
!           Find the data type, size, and base record number of the
!           last cluster in the current directory.  To do this,
!           traverse the directory record, keeping track of the record
!           count and data types of descriptors as we go.
!
            TYPE  =  DIRREC( BEGDSC )
            BASE  =  NREC + 1

            IF ( NREC .EQ. LREC ) THEN
               LINDEX = LWORD
            ELSE
               LINDEX = NWI
            END IF

            DO I = BEGDSC+2, LINDEX

               IF ( DIRREC(I) .LT. 0 ) THEN
                  TYPE = PREV(TYPE)
               ELSE
                  TYPE = NEXT(TYPE)
               END IF

               BASE  =  BASE + ABS( DIRREC(I-1) )

            END DO

!
!           TYPE and BASE are now the data type and base record number
!           of the last cluster described by the current directory.
!
!           We'll now traverse the directory in reverse order, keeping
!           track of cluster sizes and types as we go.
!
!           POS will be the index of the descriptor of the current
!           cluster.
!
            POS  =  LINDEX

            DO WHILE ( POS .GT. BEGDSC )

               IF ( POS .LT. LINDEX ) THEN
!
!                 We'll need to determine the type of the current
!                 cluster.  If the next descriptor contains a positive
!                 value, the data type of the cluster it refers to is
!                 the successor of the current type, according to our
!                 ordering of types.
!
                  IF (  DIRREC(POS+1)  .GT.  0  ) THEN
!
!                    This assignment and the one below in the ELSE
!                    block are performed from the second loop iteration
!                    onward. NXTTYP is initialized on the first loop
!                    iteration.
!
                     TYPE  =  PREV( NXTTYP )
                  ELSE
                     TYPE  =  NEXT( NXTTYP )
                  END IF

!
!                 Update the cluster base record number.
!
                  BASE    =  BASE - ABS ( DIRREC(POS) )

               END IF

!
!              Move the current cluster.
!
               DO I  =  BASE + ABS( DIRREC(POS) ) - 1,    BASE,    -1

                  IF ( TYPE .EQ. CHAR ) THEN

                     CALL DASIOC ( 'READ',   UNIT,  I,    RECC )
                     CALL DASIOC ( 'WRITE',  UNIT,  I+N,  RECC )

                  ELSE IF ( TYPE .EQ. DP ) THEN

                     CALL DASIOD ( 'READ',   UNIT,  I,    RECD )
                     CALL DASIOD ( 'WRITE',  UNIT,  I+N,  RECD )

                  ELSE

                     CALL DASIOI ( 'READ',   UNIT,  I,    RECI )
                     CALL DASIOI ( 'WRITE',  UNIT,  I+N,  RECI )

                  END IF

               END DO

!
!              The next descriptor to look at is the preceding one in
!              the directory.
!
               POS     =  POS - 1
               NXTTYP  =  TYPE

            END DO

!
!           Find the preceding directory record.
!
            PREC = DIRREC ( BWDLOC )

!
!           Update the backward and forward pointers in the current
!           directory record.  However, don't modify null pointers.
!
            IF ( DIRREC(FWDLOC) .GT. 0 ) THEN
               DIRREC(FWDLOC)  =  DIRREC(FWDLOC) + N
            END IF

            IF ( DIRREC(BWDLOC) .GT. 0 ) THEN
               DIRREC(BWDLOC)  =  DIRREC(BWDLOC) + N
            END IF

!
!           Move the current directory record.
!
            CALL DASIOI ( 'WRITE',  UNIT,  NREC+N,  DIRREC )

!
!           Consider the previous directory.
!
            NREC = PREC

         END DO

      END IF

!
!     Update the file summary.  The number of comment records and the
!     number of the first free record have been incremented by N.
!     The numbers of the records containing the last descriptor of each
!     type have been incremented by N only if they were non-zero.
!
!     The call to DASUFS will update the file record as well as the
!     file summary.
!
      NCOMR  =  NCOMR + N
      FREE   =  FREE  + N

      DO I = 1, 3

         IF ( LASTRC(I) .NE. 0 ) THEN
            LASTRC(I)  =  LASTRC(I) + N
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

      CALL CHKOUT ( 'DASACR' )
      RETURN
      END
