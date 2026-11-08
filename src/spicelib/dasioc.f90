!$Procedure DASIOC ( DAS, Fortran I/O, character )

      subroutine dasioc ( action, unit, recno, record )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Perform Fortran reads and writes of DAS character records.
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

      integer(ip)           nwc
      parameter           ( nwc = 1024 )

      character(*)          action
      integer(ip)           unit
      integer(ip)           recno
      character(nwc)        record

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ACTION     I   Action to take (read or write).
!     UNIT       I   Fortran unit connected to DAS file.
!     RECNO      I   Number of record to read or write.
!     RECORD    I-O  DAS character record.
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
!     RECORD   is a character array whose contents are to be
!              written to record RECNO, if ACTION is WRITE.
!
! $ Detailed_Output
!
!     RECORD   is a character array whose contents are to be
!              set equal to those of record RECNO, if ACTION is
!              READ.
!
! $ Parameters
!
!     NWC      is the number of characters in a DAS character
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
!     Normally, routines outside of SPICELIB will not need to call this
!     routine directly. Writes to DAS files should be performed using
!     the DASADx and DASUDx routines; reads should be performed using
!     the DASRDx routines.
!
!     This routines centralizes I/O and the concomitant error handling
!     for DAS character records.
!
!     Although most DAS routines use file handles to identify DAS
!     files, this routine uses Fortran logical units for this purpose.
!     Using unit numbers allows the DASIOx routines to be called from
!     any DAS routine, including entry points of DASFM.  (DASFM
!     contains as entry points the routines DASHLU and DASLUH, which
!     map between handles and unit numbers.)
!
! $ Examples
!
!     1)  Read and print to the screen character records number 10
!         through 20 from the DAS file designated by HANDLE.
!
!            CHARACTER*(NWC)       RECORD
!
!                           .
!                           .
!                           .
!
!            CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., UNIT )
!            CALL DASHFN   ( HANDLE, NAME )
!
!            DO I = 1, 20
!
!               CALL DASIOC ( 'READ', UNIT, 10, RECORD )
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
!     2)  Write the contents of the string RECORD to record number
!         10 in the DAS file designated by HANDLE.
!
!
!            CHARACTER*(NWC)       RECORD
!
!                           .
!                           .
!                           .
!
!            CALL ZZDDHHLU ( HANDLE,  'DAS', .FALSE., UNIT   )
!            CALL DASIOC   ( 'WRITE', UNIT,  10,      RECORD )
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
!        Header was updated to refer to ZZDDHHLU.
!
!-    SPICELIB Version 1.0.0, 30-JUN-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     perform Fortran reads of character records
!     perform Fortran writes of character records
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

            call setmsg ( 'Could not read DAS character record.  '    // &
                          'File = #  Record number = #.  IOSTAT = #.'  )
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

            call setmsg ( 'Could not write DAS character record.  '   // &
                          'File = #  Record number = #.  IOSTAT = #.'  )
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
