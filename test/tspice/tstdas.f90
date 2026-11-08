!$Procedure      TSTDAS ( Test DAS address calculation routine )

      SUBROUTINE TSTDAS ( FILE,   FTYPE,  NCOMRC, FILENO, &
                          NCLUST, CLTYPS, CLNWDS, SGRGAT, &
                          FCLOSE, NSCHEM, HANDLE         )

! $ Abstract
!
!     Create DAS files used to test the SPICELIB DAS address
!     calculation routine DASA2L.
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
!     TEST FAMILY
!     TSPICE
!     TEST UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               SCHADD
      PARAMETER           ( SCHADD = 1 )

      INTEGER               SCHCOD
      PARAMETER           ( SCHCOD = 2 )

      INTEGER               MAXCL
      PARAMETER           ( MAXCL = 20 )

      INTEGER               MAXREC
      PARAMETER           ( MAXREC = 10000 )

      INTEGER               MAXFIL
      PARAMETER           ( MAXFIL = 5000 )


      CHARACTER*(*)         FILE
      CHARACTER*(*)         FTYPE
      INTEGER               NCOMRC
      INTEGER               FILENO
      INTEGER               NCLUST
      INTEGER               CLTYPS ( * )
      INTEGER               CLNWDS ( * )
      LOGICAL               SGRGAT
      LOGICAL               FCLOSE
      INTEGER               NSCHEM
      INTEGER               HANDLE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     SCHADD     P   Generate data by address.
!     SCHCOD     P   Generate encoded data.
!     MAXCL      P   Maximum cluster size.
!     MAXCLW     P   Maximum cluster record count.
!     MAXFIL     P   Maximum file number.
!     FILE       I   Name of DAS file to create.
!     FTYPE      I   File type to use in DAS ID word.
!     NCOMRC     I   Number of comment records to reserve.
!     FILENO     I   File number used in data generation.
!     NCLUST     I   Number of data clusters.
!     CLTYPS     I   Data types of data clusters.
!     CLNWDS     I   Word counts of data clusters.
!     SGRGAT     I   Flag indicating whether to segregate file.
!     FCLOSE     I   Flag indicating whether to close file.
!     NSCHEM     I   Number scheme used for data generation.
!     HANDLE     O   DAS file handle.
!
! $ Detailed_Input
!
!     FILE       is the name of a new DAS file to create.
!
!     FTYPE      is the file type to use in the DAS ID word. This
!                string will be appended after the forward slash. The
!                maximum length of FTYPE is 4 characters.
!
!     NCOMRC     is the number of comment records to reserve in the
!                new DAS file.
!
!     FILENO     is the number of the new DAS file. This is an
!                arbitrary integer used to create file-specific
!                data when the data generation scheme is SCHCOD.
!
!     NCLUST     is the number of data clusters to write to the
!                new DAS file.
!
!     CLTYPS     is an array of cluster data types; the Ith element of
!                CLTYPS is the data type of the Ith cluster.
!
!     CLNWDS     is an array of cluster word counts; the Ith element of
!                CLNWDS is the word count of the Ith cluster. Note that
!                only the final cluster of each data type may be
!                partially filled.
!
!     SGRGAT     is a logical flag indicating whether the new file is
!                to be segregated. This flag is ignore if the file is
!                left open; in that case the file will be left in
!                in its final state after writing is complete.
!                Normally in this case the file will be unsegregated.
!
!     FCLOSE     is a logical flag indicating whether the new file is
!                to be closed. If FLCOSE is .TRUE., then SGRGAT is used
!                to determine whether the file is segregated. If the
!                file is closed, the output handle is undefined.
!
!     NSCHEM     is a parameter indicating which data generation scheme
!                is to be used. See the Parameters section below for
!                details.
!
! $ Detailed_Output
!
!     HANDLE     is a DAS handle associated with the new DAS file
!                created by this routine. HANDLE is valid only if the
!                file is left open, which happens if the input FLCOSE
!                is set to .FALSE.
!
! $ Parameters
!
!     SCHADD     indicates that each numeric word contains its own
!                address. Character words contain their addresses mod
!                128.
!
!     SCHCOD     indicates that each numeric word contains a code based
!                on the file number, record, and address.
!
!     MAXCL      is the maximum cluster size supported by this routine;
!                units are records.
!
!
!     MAXREC     is the maximum number of records in a file created by
!                this routine.
!
!     MAXFIL     is the maximum value of FILENO that is supported by
!                this routine.
!
! $ Exceptions
!
!     1)  If FILENO is larger than MAXFIL, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     2)  If an unsupported data type is found in the array CLTYPS, the
!         error SPICE(BADDATATYPE) is signaled.
!
!     3)  If a cluster word count exceeds the maximum value for
!         the cluster's data type, the error SPICE(INVALIDCOUNT)
!         is signaled. No cluster written by this routine
!         may exceed MAXCL records.
!
!     4)  If a cluster is not the last of its data type and has
!         a word count that does not correspond to an integer
!         number of data records of that type, the error
!         SPICE(INVALIDCOUNT) is signaled.
!
! $ Files
!
!     This routine creates DAS files having specified cluster types
!     and sizes. The caller can control the scheme used to create
!     data via the data scheme input NSCHEM.
!
!     The caller can indicate whether the file to be created should
!     be left open or closed, and if the file is closed, whether it
!     is to be segregated.
!
! $ Particulars
!
!     See the Files and Parameters sections above.
!
! $ Examples
!
!     See use in F_DASA2L.
!
! $ Restrictions
!
!     This is a SPICE test routine; its specification and interface
!     may be updated without notice.
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
!-    TSPICE Version 1.1.0, 13-FEB-2015 (NJB)
!
!        Bug fix: now writes buffered records via DASWBR
!        in the branch that performs an unsegregated close.
!
!        Bug fix: test of file count against MAXFIL now uses
!        strict inequality (.GT.) rather than .GE.
!
!        Updated MAXFIL to 5000.
!
!
!-    TSPICE Version 1.0.0, 11-APR-2014 (NJB)
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
      INTEGER               CHR
      PARAMETER           ( CHR    =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )



!     These parameters are named to enhance ease of maintenance of
!     the code; the values should not be changed.

      INTEGER               NWD
      PARAMETER           ( NWD    =  128 )

      INTEGER               NWI
      PARAMETER           ( NWI    =  256 )

      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )

!
!     File number factor used for creating data:
!
      INTEGER               FILEX
      PARAMETER           ( FILEX  =   100000 )

      INTEGER               RECX
      PARAMETER           ( RECX   =     1000 )

!
!     Local variables
!
      CHARACTER*(NWC)       CREC   ( MAXCL )

      DOUBLE PRECISION      DREC   ( NWD, MAXCL )

      INTEGER               CLIDX
      INTEGER               I
      INTEGER               IREC   ( NWI, MAXCL )
      INTEGER               J
      INTEGER               K
      INTEGER               NCL    ( 3 )
      INTEGER               NW
      INTEGER               NWREC  ( 3 )
      INTEGER               NWRITE
      INTEGER               NWRITN ( 3 )
      INTEGER               RECNO
      INTEGER               REMAIN
      INTEGER               TYPE

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  NWREC / NWC, NWD, NWI /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TSTDAS' )

      IF ( FILENO .GT. MAXFIL ) THEN

         CALL SETMSG ( 'FILENO = #; max allowed value is #.' )
         CALL ERRINT ( '#', FILENO                           )
         CALL ERRINT ( '#', MAXFIL                           )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'              )
         CALL CHKOUT ( 'TSTDAS'                              )
         RETURN

      END IF

!
!     Check cluster data types and sizes.
!
      CALL CLEARI ( 3, NCL )

      DO CLIDX = NCLUST, 1, -1

         TYPE = CLTYPS(CLIDX)
!
!        Check the data type before using it as an array index.
!
         IF (  ( TYPE .LT. CHR ) .OR. ( TYPE .GT. INT )  ) THEN

            CALL SETMSG ( 'Invalid data type code # found for ' &
            //            'cluster #.'                         )
            CALL ERRINT ( '#', TYPE                            )
            CALL ERRINT ( '#', CLIDX                           )
            CALL SIGERR ( 'SPICE(BADDATATYPE)'                 )
            CALL CHKOUT ( 'TSTDAS'                             )
            RETURN

         END IF

!
!        Check the cluster size. The upper bound check is
!        loose, but it may catch uninitialized junk.
!
         I = CLNWDS(CLIDX)

         IF (       ( I .LT. 1                 ) &
               .OR. ( I .GT. MAXCL*NWREC(TYPE) )  ) THEN

            CALL SETMSG ( 'Invalid cluster word count # found for ' &
            //            'cluster #.'                             )
            CALL ERRINT ( '#', I                                   )
            CALL ERRINT ( '#', CLIDX                               )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
            CALL CHKOUT ( 'TSTDAS'                                 )
            RETURN

         END IF

!
!        The cluster must be "full" unless this is
!        the final cluster of this data type. Check it.
!
         NCL(TYPE) = NCL(TYPE) + 1

         NW        = NWREC(TYPE)

         IF ( (I/NW)*NW  .NE. I ) THEN
!
!           The word count is not an integral multiple
!           of the number of words of TYPE that fit in a
!           record.
!
            IF ( NCL(TYPE) .GT. 1 ) THEN
!
!              We're searching backward in the cluster lists,
!              so a cluster count greater than 1 means we're
!              not looking at the last cluster of data type
!              TYPE.
!
               CALL SETMSG ( 'Partial cluster found prior to ' &
               //            'last cluster of data type #. ' &
               //            'Cluster index is #. Cluster word ' &
               //            'count is #.'                      )
               CALL ERRINT ( '#', TYPE                          )
               CALL ERRINT ( '#', CLIDX                         )
               CALL ERRINT ( '#', I                             )
               CALL SIGERR ( 'SPICE(INVALIDCOUNT)'              )
               CALL CHKOUT ( 'TSTDAS'                           )
               RETURN

            END IF

         END IF

      END DO

!
!     Open the file.
!
      CALL DASONW ( FILE, FTYPE, FILE, NCOMRC, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TSTDAS' )
         RETURN
      END IF

!
!     Write data to the file.
!
      CALL CLEARI ( 3, NWRITN )

      RECNO  = 1

      DO CLIDX = 1, NCLUST

         IF ( RECNO .GE. MAXREC ) THEN

            CALL SETMSG ( 'RECNO = #; max allowed value is #.' )
            CALL ERRINT ( '#', RECNO                           )
            CALL ERRINT ( '#', MAXREC                          )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'             )
            CALL CHKOUT ( 'TSTDAS'                             )
            RETURN

         END IF

         TYPE = CLTYPS(CLIDX)
         NW   = NWREC (TYPE )

!
!        Create data current cluster, one record at a time. Write
!        the entire cluster using a single call.
!
         IF ( TYPE .EQ. INT ) THEN
!
!           Populate a buffer with data of the indicated type.
!
            REMAIN = CLNWDS(CLIDX)
            J      = 1

            DO WHILE ( REMAIN .GT. 0 )

               NWRITE = MIN ( NW, REMAIN )

               DO I = 1, NWRITE

                  IF ( NSCHEM .EQ. SCHADD ) THEN

                     IREC(I,J) = I + NWRITN(TYPE)

                  ELSE

                     IREC(I,J) =   ( FILENO * FILEX ) &
                                 + ( RECNO  * RECX  ) &
                                 +   I
                  END IF

               END DO

               NWRITN(TYPE) = NWRITN(TYPE) + NWRITE

               J      = J      + 1
               RECNO  = RECNO  + 1
               REMAIN = REMAIN - NWRITE

            END DO

            CALL DASADI ( HANDLE, CLNWDS(CLIDX), IREC )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TSTDAS' )
               RETURN
            END IF


         ELSE IF ( TYPE .EQ. DP ) THEN

            REMAIN = CLNWDS(CLIDX)
            J      = 1

            DO WHILE ( REMAIN .GT. 0 )

               NWRITE = MIN ( NW, REMAIN )

               DO I = 1, NWRITE

                  IF ( NSCHEM .EQ. SCHADD ) THEN

                     DREC(I,J) = DBLE(I) + NWRITN(TYPE)

                  ELSE
                     DREC(I,J) =   ( FILENO * FILEX ) &
                                 + ( RECNO  * RECX  ) &
                                 +   DBLE(I)
                  END IF

               END DO

               NWRITN(TYPE) = NWRITN(TYPE) + NWRITE


               J      = J      + 1
               RECNO  = RECNO  + 1
               REMAIN = REMAIN - NWRITE

            END DO

            CALL DASADD ( HANDLE, CLNWDS(CLIDX), DREC )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TSTDAS' )
               RETURN
            END IF


         ELSE IF ( TYPE .EQ. CHR ) THEN

            REMAIN = CLNWDS(CLIDX)
            J      = 1

            DO WHILE ( REMAIN .GT. 0 )

               NWRITE = MIN ( NW, REMAIN )

               DO I = 1, NWRITE

                  IF ( NSCHEM .EQ. SCHADD ) THEN

                     K      = I + NWRITN(TYPE)
                  ELSE
                     K      =   ( FILENO * FILEX ) &
                              + ( RECNO  * RECX  ) &
                              +   I
                  END IF

                  CREC(J)(I:I) = CHAR(  MOD( K, 128 )  )

               END DO

               NWRITN(TYPE) = NWRITN(TYPE) + NWRITE


               J      = J      + 1
               RECNO  = RECNO  + 1
               REMAIN = REMAIN - NWRITE

            END DO

            CALL DASADC ( HANDLE, CLNWDS(CLIDX), 1, NWC, CREC )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TSTDAS' )
               RETURN
            END IF

         ELSE

            CALL SETMSG ( 'Bad data type # found for ' &
            //            'cluster #.'                )
            CALL ERRINT ( '#', TYPE                   )
            CALL ERRINT ( '#', CLIDX                  )
            CALL SIGERR ( 'SPICE(BADDATATYPE)'        )
            CALL CHKOUT ( 'TSTDAS'                    )
            RETURN

         END IF

      END DO


      IF ( FCLOSE ) THEN
!
!        Close the file. Segregate the records if so commanded.
!
         IF ( SGRGAT ) THEN

            CALL DASCLS ( HANDLE )
         ELSE
            CALL DASWBR ( HANDLE )
            CALL DASLLC ( HANDLE )
         END IF

      END IF

      CALL CHKOUT ( 'TSTDAS' )
      RETURN
      END
