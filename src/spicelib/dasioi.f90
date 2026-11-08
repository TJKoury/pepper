!$Procedure DASIOI ( DAS, Fortran I/O, integer )

      subroutine dasioi ( action, unit, recno, record )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Perform Fortran reads and writes of DAS integer records.
!     This routine operates on DAS files having native binary
!     format.
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

      implicit none

      integer(ip), parameter :: nwi = 256

      character(*)          action
      integer(ip)           unit
      integer(ip)           recno
      integer(ip)           record ( nwi )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action to take (read or write).
!     UNIT       I   Fortran unit connected to DAS file.
!     RECNO      I   Number of record to read or write.
!     RECORD    I-O  DAS integer record.
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
!     RECORD   is an integer array whose contents are to be
!              written to record RECNO, if ACTION is WRITE.
!
! $ Detailed_Output
!
!     RECORD   is an integer array whose contents are to be
!              set equal to those of record RECNO, if ACTION
!              is READ.
!
! $ Parameters
!
!     NWI      is the number of elements in a DAS integer record.
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
!     routine ZZDASGDI should be used to read integer records from DAS
!     files that may have either native or non-native format.
!
!     Normally, routines outside of SPICELIB will not need to call this
!     routine directly. Writes to DAS files should be performed using
!     the DASADx and DASUDx routines; reads should be performed using
!     the DASRDx routines.
!
!     This routine centralizes I/O and the concomitant error handling
!     for DAS integer records.
!
!     Although most DAS routines use file handles to identify DAS
!     files, this routine uses Fortran logical units for this purpose.
!     Using unit numbers allows the DASIOx routines to be called from
!     any DAS routine, including entry points of DASFM.
!
! $ Examples
!
!     1)  Read and print to the screen integer records number 10
!         through 20 from the DAS file designated by HANDLE.
!
!            INTEGER               RECORD ( NWI )
!                           .
!                           .
!                           .
!
!            CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )
!            CALL DASHFN   ( HANDLE, NAME )
!
!            DO I = 1, 20
!
!               CALL DASIOI ( 'READ', UNIT, 10, RECORD )
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
!            INTEGER               RECORD ( NWI )
!
!                           .
!                           .
!                           .
!
!            CALL ZZDDHHLU ( HANDLE,  'DAS', .FALSE., UNIT   )
!            CALL DASIOI   ( 'WRITE', UNIT,  10,      RECORD )
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
!     perform Fortran reads of integer records
!     perform Fortran writes of integer records
!     perform low-level I/O for DAS routines
!
!-&


!
!     SPICELIB functions
!
      logical               eqstr
      logical               return

!
!     Local variables
!
      integer(ip)           iostat


!
!     Use discovery check-in.
!
      if ( return() ) return


      if (   eqstr ( action, 'READ' )   ) then
!
!        We're supposed to read the file.
!
         read (  unit    =    unit, &
                 rec     =    recno, &
                 iostat  =    iostat  )    record

         if ( iostat /= 0 ) then

            call setmsg ( 'Could not read DAS integer record. '       // &
                          'File = # Record number = #. IOSTAT = #.'    )
            call errfnm ( '#', unit                                    )
            call errint ( '#', recno                                   )
            call errint ( '#', iostat                                  )
            call sigerr ( 'SPICE(DASFILEREADFAILED)'                   )
            return

         end if


      else if (  eqstr ( action, 'WRITE' )  ) then

!
!        We're supposed to write to the file.
!
         write (  unit    =    unit, &
                  rec     =    recno, &
                  iostat  =    iostat  )    record

         if ( iostat /= 0 ) then

            call setmsg ( 'Could not write DAS integer record. '      // &
                          'File = # Record number = #. IOSTAT = #.'    )
            call errfnm ( '#', unit                                    )
            call errint ( '#', recno                                   )
            call errint ( '#', iostat                                  )
            call sigerr ( 'SPICE(DASFILEWRITEFAILED)'                  )
            return

         end if


      else
!
!        The requested action is a little too weird.
!
         call setmsg ( 'Action was #; should be READ or WRITE' )
         call errch  ( '#', action                             )
         call sigerr ( 'SPICE(UNRECOGNIZEDACTION)'             )
         return

      end if


      return
      end
