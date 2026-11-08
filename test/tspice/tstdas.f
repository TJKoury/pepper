C$Procedure      TSTDAS ( Test DAS address calculation routine )
 
      SUBROUTINE TSTDAS ( FILE,   FTYPE,  NCOMRC, FILENO,
     .                    NCLUST, CLTYPS, CLNWDS, SGRGAT,
     .                    FCLOSE, NSCHEM, HANDLE         )

C$ Abstract
C
C     Create DAS files used to test the SPICELIB DAS address 
C     calculation routine DASA2L.
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
C     DAS
C
C$ Keywords
C
C     DAS
C     TEST FAMILY
C     TSPICE
C     TEST UTILITY
C
C$ Declarations
 
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
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     SCHADD     P   Generate data by address.
C     SCHCOD     P   Generate encoded data.
C     MAXCL      P   Maximum cluster size.
C     MAXCLW     P   Maximum cluster record count.
C     MAXFIL     P   Maximum file number.
C     FILE       I   Name of DAS file to create.   
C     FTYPE      I   File type to use in DAS ID word.
C     NCOMRC     I   Number of comment records to reserve.
C     FILENO     I   File number used in data generation.
C     NCLUST     I   Number of data clusters.
C     CLTYPS     I   Data types of data clusters.
C     CLNWDS     I   Word counts of data clusters.
C     SGRGAT     I   Flag indicating whether to segregate file.
C     FCLOSE     I   Flag indicating whether to close file.
C     NSCHEM     I   Number scheme used for data generation.
C     HANDLE     O   DAS file handle.
C
C$ Detailed_Input
C
C     FILE       is the name of a new DAS file to create.
C
C     FTYPE      is the file type to use in the DAS ID word. This
C                string will be appended after the forward slash. The
C                maximum length of FTYPE is 4 characters.
C
C     NCOMRC     is the number of comment records to reserve in the
C                new DAS file.
C
C     FILENO     is the number of the new DAS file. This is an 
C                arbitrary integer used to create file-specific
C                data when the data generation scheme is SCHCOD.
C
C     NCLUST     is the number of data clusters to write to the 
C                new DAS file.
C
C     CLTYPS     is an array of cluster data types; the Ith element of
C                CLTYPS is the data type of the Ith cluster.
C
C     CLNWDS     is an array of cluster word counts; the Ith element of
C                CLNWDS is the word count of the Ith cluster. Note that
C                only the final cluster of each data type may be 
C                partially filled. 
C
C     SGRGAT     is a logical flag indicating whether the new file is
C                to be segregated. This flag is ignore if the file is
C                left open; in that case the file will be left in
C                in its final state after writing is complete. 
C                Normally in this case the file will be unsegregated.
C
C     FCLOSE     is a logical flag indicating whether the new file is
C                to be closed. If FLCOSE is .TRUE., then SGRGAT is used
C                to determine whether the file is segregated. If the 
C                file is closed, the output handle is undefined.
C
C     NSCHEM     is a parameter indicating which data generation scheme
C                is to be used. See the Parameters section below for
C                details.
C
C$ Detailed_Output
C
C     HANDLE     is a DAS handle associated with the new DAS file
C                created by this routine. HANDLE is valid only if the
C                file is left open, which happens if the input FLCOSE
C                is set to .FALSE.
C
C$ Parameters
C
C     SCHADD     indicates that each numeric word contains its own
C                address. Character words contain their addresses mod
C                128.
C
C     SCHCOD     indicates that each numeric word contains a code based
C                on the file number, record, and address.
C
C     MAXCL      is the maximum cluster size supported by this routine;
C                units are records.
C
C
C     MAXREC     is the maximum number of records in a file created by
C                this routine.
C
C     MAXFIL     is the maximum value of FILENO that is supported by
C                this routine.
C
C$ Exceptions
C
C     1)  If FILENO is larger than MAXFIL, the error
C         SPICE(VALUEOUTOFRANGE) is signaled.
C
C     2)  If an unsupported data type is found in the array CLTYPS, the
C         error SPICE(BADDATATYPE) is signaled.
C
C     3)  If a cluster word count exceeds the maximum value for
C         the cluster's data type, the error SPICE(INVALIDCOUNT)
C         is signaled. No cluster written by this routine 
C         may exceed MAXCL records.
C
C     4)  If a cluster is not the last of its data type and has
C         a word count that does not correspond to an integer
C         number of data records of that type, the error 
C         SPICE(INVALIDCOUNT) is signaled.
C
C$ Files
C
C     This routine creates DAS files having specified cluster types
C     and sizes. The caller can control the scheme used to create
C     data via the data scheme input NSCHEM.
C
C     The caller can indicate whether the file to be created should
C     be left open or closed, and if the file is closed, whether it
C     is to be segregated.
C
C$ Particulars
C
C     See the Files and Parameters sections above.
C
C$ Examples
C
C     See use in F_DASA2L.
C
C$ Restrictions
C
C     This is a SPICE test routine; its specification and interface
C     may be updated without notice. 
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
C-    TSPICE Version 1.1.0, 13-FEB-2015 (NJB)
C
C        Bug fix: now writes buffered records via DASWBR
C        in the branch that performs an unsegregated close.
C
C        Bug fix: test of file count against MAXFIL now uses
C        strict inequality (.GT.) rather than .GE.
C
C        Updated MAXFIL to 5000.
C     
C     
C-    TSPICE Version 1.0.0, 11-APR-2014 (NJB)
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               FAILED
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               CHR
      PARAMETER           ( CHR    =  1  )

      INTEGER               DP
      PARAMETER           ( DP     =  2  )

      INTEGER               INT
      PARAMETER           ( INT    =  3  )


 
C     These parameters are named to enhance ease of maintenance of
C     the code; the values should not be changed.
 
      INTEGER               NWD
      PARAMETER           ( NWD    =  128 )
 
      INTEGER               NWI
      PARAMETER           ( NWI    =  256 )
 
      INTEGER               NWC
      PARAMETER           ( NWC    = 1024 )

C
C     File number factor used for creating data:
C      
      INTEGER               FILEX
      PARAMETER           ( FILEX  =   100000 )

      INTEGER               RECX
      PARAMETER           ( RECX   =     1000 )

C
C     Local variables
C    
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

C
C     Saved variables
C
      SAVE

C
C     Initial values
C     
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

C
C     Check cluster data types and sizes.
C
      CALL CLEARI ( 3, NCL )

      DO CLIDX = NCLUST, 1, -1

         TYPE = CLTYPS(CLIDX)
C
C        Check the data type before using it as an array index.
C         
         IF (  ( TYPE .LT. CHR ) .OR. ( TYPE .GT. INT )  ) THEN

            CALL SETMSG ( 'Invalid data type code # found for '
     .      //            'cluster #.'                         )
            CALL ERRINT ( '#', TYPE                            )
            CALL ERRINT ( '#', CLIDX                           )
            CALL SIGERR ( 'SPICE(BADDATATYPE)'                 )
            CALL CHKOUT ( 'TSTDAS'                             )
            RETURN

         END IF

C
C        Check the cluster size. The upper bound check is
C        loose, but it may catch uninitialized junk.
C
         I = CLNWDS(CLIDX) 

         IF (       ( I .LT. 1                 )
     .         .OR. ( I .GT. MAXCL*NWREC(TYPE) )  ) THEN

            CALL SETMSG ( 'Invalid cluster word count # found for '
     .      //            'cluster #.'                             )
            CALL ERRINT ( '#', I                                   )
            CALL ERRINT ( '#', CLIDX                               )
            CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
            CALL CHKOUT ( 'TSTDAS'                                 )
            RETURN

         END IF

C
C        The cluster must be "full" unless this is
C        the final cluster of this data type. Check it.
C
         NCL(TYPE) = NCL(TYPE) + 1

         NW        = NWREC(TYPE)
         
         IF ( (I/NW)*NW  .NE. I ) THEN
C
C           The word count is not an integral multiple
C           of the number of words of TYPE that fit in a 
C           record.
C
            IF ( NCL(TYPE) .GT. 1 ) THEN
C
C              We're searching backward in the cluster lists,
C              so a cluster count greater than 1 means we're
C              not looking at the last cluster of data type
C              TYPE.
C
               CALL SETMSG ( 'Partial cluster found prior to '
     .         //            'last cluster of data type #. '
     .         //            'Cluster index is #. Cluster word '
     .         //            'count is #.'                      )
               CALL ERRINT ( '#', TYPE                          )
               CALL ERRINT ( '#', CLIDX                         )
               CALL ERRINT ( '#', I                             )
               CALL SIGERR ( 'SPICE(INVALIDCOUNT)'              )
               CALL CHKOUT ( 'TSTDAS'                           )
               RETURN

            END IF

         END IF 

      END DO

C
C     Open the file.
C
      CALL DASONW ( FILE, FTYPE, FILE, NCOMRC, HANDLE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'TSTDAS' )
         RETURN
      END IF

C
C     Write data to the file. 
C     
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

C
C        Create data current cluster, one record at a time. Write
C        the entire cluster using a single call.
C
         IF ( TYPE .EQ. INT ) THEN
C
C           Populate a buffer with data of the indicated type.
C            
            REMAIN = CLNWDS(CLIDX)
            J      = 1

            DO WHILE ( REMAIN .GT. 0 )

               NWRITE = MIN ( NW, REMAIN )
               
               DO I = 1, NWRITE
                                    
                  IF ( NSCHEM .EQ. SCHADD ) THEN

                     IREC(I,J) = I + NWRITN(TYPE)

                  ELSE 

                     IREC(I,J) =   ( FILENO * FILEX ) 
     ,                           + ( RECNO  * RECX  )
     .                           +   I
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
                     DREC(I,J) =   ( FILENO * FILEX ) 
     ,                           + ( RECNO  * RECX  )
     .                           +   DBLE(I)
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
                     K      =   ( FILENO * FILEX ) 
     .                        + ( RECNO  * RECX  )
     .                        +   I
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

            CALL SETMSG ( 'Bad data type # found for '
     .      //            'cluster #.'                )
            CALL ERRINT ( '#', TYPE                   )
            CALL ERRINT ( '#', CLIDX                  )
            CALL SIGERR ( 'SPICE(BADDATATYPE)'        )
            CALL CHKOUT ( 'TSTDAS'                    )
            RETURN

         END IF

      END DO


      IF ( FCLOSE ) THEN
C
C        Close the file. Segregate the records if so commanded.
C
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
