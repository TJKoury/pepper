!$Procedure ZZDASGRI ( DAS, get record, integer )

      subroutine zzdasgri ( handle, recno, record )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Read DAS integer physical records from native or non-native DAS
!     files.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     DAS
!     FILES
!
! $ Declarations

      implicit none

      include 'das.inc'

      integer(ip)           handle
      integer(ip)           recno
      integer(ip)           record ( nwi )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of DAS file.
!     RECNO      I   Record number.
!     RECORD     O   Integer data read from record.
!     NWI        P   Number of integers in a single DAS integer record.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of an open DAS file.
!
!     RECNO          is the number of a record in a DAS file.
!
! $ Detailed_Output
!
!     RECORD         is an integer array containing the elements of the
!                    specified record.
!
! $ Parameters
!
!     NWI            is the number of elements in a single DAS integer
!                    data record.
!
! $ Exceptions
!
!     1)  If the input file handle cannot be mapped to a file
!         name, the error SPICE(HANDLENOTFOUND) will be signaled. The
!         output argument RECORD will not be modified.
!
!     2)  If a read operation attempted by this routine fails, the
!         error SPICE(DASFILEREADFAILED) will be signaled.
!
!     3)  If an error occurs while attempting to translate non-native
!         integer data to native format, the error will be diagnosed by
!         a routine in the call tree of this routine.
!
! $ Files
!
!     See the description of the argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Routines outside of SPICELIB will normally have no need to call
!     this routine.
!
!     This routine enables DAS routines to read double precision data
!     records from native or non-native DAS files.
!
!     This routine should be used to read only records that contain
!     integer data.
!
! $ Examples
!
!     See usage in DASRRI.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 10-FEB-2017 (NJB)
!
!-&

! $ Index_Entries
!
!     read DAS integer physical records for arbitrary BFF
!     read DAS integer records for native or non-native DAS
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           filsiz
      parameter           ( filsiz = 255 )

!
!     Local variables
!
      character(nwc)        chrrec
      character(filsiz)     fname

      integer(ip)           intamh
      integer(ip)           intarc
      integer(ip)           intbff
      integer(ip)           iostat
      integer(ip)           natbff
      integer(ip)           unit

      logical               first
      logical               found

!
!     Saved variables
!
      save                 first
      save                 intbff
      save                 natbff


!
!     Initial values
!
      data                 first  / .true. /


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZDASGRI' )

      if ( first ) then
!
!        Get the integer code for the host binary file format.
!
         call zzddhnfc ( natbff )

         if ( failed() ) then
            call chkout( 'ZZDASGRI' )
            return
         end if

         first = .false.

      end if

!
!     Get a logical unit for this file.
!
      call zzddhhlu ( handle, 'DAS', .false., unit )
!
!     Get the binary file format of the file designated by HANDLE.
!
      call zzddhnfo ( handle, fname, intarc, intbff, intamh, found )

      if ( failed() ) then
         call chkout( 'ZZDASGRI' )
         return
      end if

      if ( .not. found ) then

         call setmsg ( 'Unable to locate file associated with ' &
         //            'HANDLE, #. The most likely cause of this ' &
         //            'is the file that you are trying to read ' &
         //            'has been closed.'                           )
         call errint ( '#', handle                                  )
         call sigerr ( 'SPICE(HANDLENOTFOUND)'                      )
         call chkout ( 'ZZDASGRI'                                   )
         return

      end if


      if ( intbff == natbff ) then
!
!        The file has native format.
!
         read (  unit    =   unit, &
                 rec     =   recno, &
                 iostat  =   iostat  ) record

         if ( iostat /= 0 ) then

            call setmsg ( 'Could not read DAS integer record. ' &
            //            'File = # Record number = #. IOSTAT = #.'    )
            call errfnm ( '#', unit                                    )
            call errint ( '#', recno                                   )
            call errint ( '#', iostat                                  )
            call sigerr ( 'SPICE(DASFILEREADFAILED)'                   )
            call chkout ( 'ZZDASGRI'                                   )
            return

         end if

      else
!
!        Read the record as a character string, then translate it
!        to an array of integers.
!
         read (  unit    =    unit, &
                 rec     =    recno, &
                 iostat  =    iostat  ) chrrec

         if ( iostat /= 0 ) then

            call setmsg ( 'Could not read non-native DAS integer ' &
            //            'record into character array. ' &
            //            'File = # Record number = #. IOSTAT = #.'    )
            call errfnm ( '#', unit                                    )
            call errint ( '#', recno                                   )
            call errint ( '#', iostat                                  )
            call sigerr ( 'SPICE(DASFILEREADFAILED)'                   )
            call chkout ( 'ZZDASGRI'                                   )
            return

         end if

!
!        Translate the character record to integer type.
!
         call zzxlatei ( intbff, chrrec, nwi, record )

!
!        We don't test FAILED here because the routine
!        will return from this point.
!
      end if

      call chkout ( 'ZZDASGRI' )
      return
      end
