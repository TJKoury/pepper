!$Procedure DASIOD ( DAS, Fortran I/O, double precision )

      SUBROUTINE DASIOD ( ACTION, UNIT, RECNO, RECORD )

! $ Abstract
!
!     Perform Fortran reads and writes of DAS double precision records.
!     This routine operates on DAS files having native binary format.
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

      INTEGER               NWD
      PARAMETER           ( NWD = 128 )

      CHARACTER*(*)         ACTION
      INTEGER               UNIT
      INTEGER               RECNO
      DOUBLE PRECISION      RECORD ( NWD )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action to take (read or write).
!     UNIT       I   Fortran unit connected to DAS file.
!     RECNO      I   Number of record to read or write.
!     RECORD    I-O  DAS double precision record.
!
! $ Detailed_Input
!
!     ACTION   is a character string specifying whether to read
!              from or write to the specified DAS file. Possible
!              values are:
!
!                 'READ'
!                 'WRITE'
!
!              Case and leading or trailing blanks are not
!              significant.
!
!
!     UNIT     is the Fortran unit number connected to the DAS
!              file that is to be read or written. Given the
!              handle of the DAS file, the unit number can be
!              obtained using ZZDDHHLU.
!
!     RECNO    is the Fortran record number of the record to be
!              read or written.
!
!     RECORD   is a double precision array whose contents are to
!              be written to record RECNO, if ACTION is WRITE.
!
! $ Detailed_Output
!
!     RECORD   is a double precision array whose contents are to
!              be set equal to those of record RECNO, if ACTION
!              is READ.
!
! $ Parameters
!
!     NWD      is the number of elements in a DAS double precision
!              record.
!
! $ Exceptions
!
!     1)  If the value of ACTION is not recognized, the error
!         SPICE(UNRECOGNIZEDACTION) is signaled.
!
!     2)  If a Fortran read error occurs, the error
!         SPICE(DASFILEREADFAILED) is signaled.
!
!     3)  If a Fortran write error occurs, the error
!         SPICE(DASFILEWRITEFAILED) is signaled.
!
! $ Files
!
!     See the description of the argument UNIT in $Detailed_Input.
!
! $ Particulars
!
!     This routine may be used to write to and read from DAS files
!     having the native binary file format of the host system. The
!     routine ZZDASGDR should be used to read double precision records
!     from DAS files that may have either native or non-native format.
!
!     Normally, routines outside of SPICELIB will not need to call this
!     routine directly. Writes to DAS files should be performed using
!     the DASADx and DASUDx routines; reads should be performed using
!     the DASRDx routines.
!
!     This routine centralizes I/O and the concomitant error handling
!     for DAS double precision records.
!
!     Although most DAS routines use file handles to identify DAS
!     files, this routine uses Fortran logical units for this purpose.
!     Using unit numbers allows the DASIOx routines to be called from
!     any DAS routine, including entry points of DASFM.
!
! $ Examples
!
!     1)  Read and print to the screen double precision records
!         number 10 through 20 from the DAS file designated by HANDLE.
!
!
!            DOUBLE PRECISION      RECORD ( NWD )
!                           .
!                           .
!                           .
!
!            CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )
!            CALL DASHFN   ( HANDLE, NAME )
!
!            DO I = 1, 20
!
!               CALL DASIOD ( 'READ', UNIT, 10, RECORD )
!
!               LABEL = 'Contents of the # record in DAS file #: '
!
!               CALL REPMOT ( LABEL,  '#',  I,  'L',   LABEL )
!               CALL REPMC  ( LABEL,  '#',      NAME,  LABEL )
!
!               WRITE (*,*) LABEL
!               WRITE (*,*) ' '
!               WRITE (*,*) RECORD
!
!            END DO
!
!
!
!     2)  Write the contents of the array RECORD to record number
!         10 in the DAS file designated by HANDLE.
!
!
!            DOUBLE PRECISION      RECORD ( NWD )
!
!                           .
!                           .
!                           .
!
!            CALL ZZDDHHLU ( HANDLE,  'DAS', .FALSE., UNIT   )
!            CALL DASIOD   ( 'WRITE', UNIT,  10,      RECORD )
!
! $ Restrictions
!
!     1)  This routine may be used only on DAS files having
!         the native binary file format of the host system.
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
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 05-FEB-2015 (NJB)
!
!        Header was updated to refer to ZZDDHHLU. $Restrictions section
!        was updated.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     perform Fortran reads of double precision records
!     perform Fortran writes of double precision records
!     perform low-level I/O for DAS routines
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               EQSTR
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               IOSTAT


!
!     Use discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF


      IF (   EQSTR ( ACTION, 'READ' )   ) THEN
!
!        We're supposed to read the file.
!
         READ (  UNIT    =    UNIT, &
                 REC     =    RECNO, &
                 IOSTAT  =    IOSTAT  )    RECORD

         IF ( IOSTAT .NE. 0 ) THEN

            CALL CHKIN  ( 'DASIOD'                                     )
            CALL SETMSG ( 'Could not read DAS double precision '      // &
                          'record. File = # Record number = #. '      // &
                          'IOSTAT = #.'                                )
            CALL ERRFNM ( '#', UNIT                                    )
            CALL ERRINT ( '#', RECNO                                   )
            CALL ERRINT ( '#', IOSTAT                                  )
            CALL SIGERR ( 'SPICE(DASFILEREADFAILED)'                   )
            CALL CHKOUT ( 'DASIOD'                                     )
            RETURN

         END IF


      ELSE IF (  EQSTR ( ACTION, 'WRITE' )  ) THEN

!
!        We're supposed to write to the file.
!
         WRITE (  UNIT    =    UNIT, &
                  REC     =    RECNO, &
                  IOSTAT  =    IOSTAT  )    RECORD

         IF ( IOSTAT .NE. 0 ) THEN

            CALL CHKIN  ( 'DASIOD'                                     )
            CALL SETMSG ( 'Could not write DAS double precision '     // &
                          'record. File = # Record number = #. '      // &
                          'IOSTAT = #.'                                )
            CALL ERRFNM ( '#', UNIT                                    )
            CALL ERRINT ( '#', RECNO                                   )
            CALL ERRINT ( '#', IOSTAT                                  )
            CALL SIGERR ( 'SPICE(DASFILEWRITEFAILED)'                  )
            CALL CHKOUT ( 'DASIOD'                                     )
            RETURN

         END IF


      ELSE
!
!        The requested action is a little too weird.
!
         CALL CHKIN  ( 'DASIOD'                                )
         CALL SETMSG ( 'Action was #; should be READ or WRITE' )
         CALL ERRCH  ( '#', ACTION                             )
         CALL SIGERR ( 'SPICE(UNRECOGNIZEDACTION)'             )
         CALL CHKOUT ( 'DASIOD'                                )
         RETURN

      END IF


      RETURN
      END
