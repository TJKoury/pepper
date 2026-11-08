!$Procedure      ZZDASRFR ( DAS, read file record )

      subroutine zzdasrfr ( handle, &
                            idword, &
                            ifname, &
                            nresvr, &
                            nresvc, &
                            ncomr, &
                            ncomc  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the contents of the file record of a specified DAS file.
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

      include 'zzddhman.inc'

      integer(ip)           handle
      character(*)          idword
      character(*)          ifname
      integer(ip)           nresvr
      integer(ip)           nresvc
      integer(ip)           ncomr
      integer(ip)           ncomc

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   DAS file handle.
!     IDWORD     O   ID word.
!     IFNAME     O   DAS internal file name.
!     NRESVR     O   Number of reserved records in file.
!     NRESVC     O   Number of characters in use in reserved rec. area.
!     NCOMR      O   Number of comment records in file.
!     NCOMC      O   Number of characters in use in comment area.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle for a previously opened DAS file.
!
! $ Detailed_Output
!
!     IDWORD      is the `ID word' contained in the first eight
!                 characters of the file record.
!
!     IFNAME      is the internal file name of the DAS file.  The
!                 maximum length of the internal file name is 60
!                 characters.
!
!     NRESVR      is the number of reserved records in the DAS file
!                 specified by HANDLE.
!
!     NRESVC      is the number of characters in use in the reserved
!                 record area of the DAS file specified by HANDLE.
!
!     NCOMR       is the number of comment records in the DAS file
!                 specified by HANDLE.
!
!     NCOMC       is the number of characters in use in the comment area
!                 of the DAS file specified by HANDLE.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the file read attempted by this routine fails, the error
!        SPICE(DASFILEREADFAILED) will be signaled.
!
!     2) If the input file handle is invalid, the error will diagnosed
!        by a routine in the call tree of this routine.
!
!     3) If a logical unit cannot be obtained for the file designated
!        by HANDLE, the error will diagnosed by a routine in the call
!        tree of this routine.
!
!     4) If the file's binary format is unrecognized, the error will
!        diagnosed by a routine in the call tree of this routine.
!
!     5) If the file designated by HANDLE has non-native binary format,
!        and if any numeric components of the file record cannot be
!        translated to native format, the error will diagnosed
!        by a routine in the call tree of this routine.
!
! $ Files
!
!     See the description of HANDLE under $Detailed_Input.
!
! $ Particulars
!
!     This routine provides a convenient way of retrieving the
!     information contained in the file record of a DAS file.
!
! $ Examples
!
!     1)  Obtain the internal file name of an existing DAS file.
!
!
!            C
!            C     Open the file for reading.
!            C
!                  CALL DASOPR ( FNAME, HANDLE  )
!
!            C
!            C     Retrieve the internal file name and print it.
!            C
!
!                  CALL ZZDASRFR ( HANDLE,
!                 .                IDWORD,
!                 .                IFNAME,
!                 .                NRESVR,
!                 .                NRESVC,
!                 .                NCOMR,
!                 .                NCOMC  )
!
!
!                  WRITE (*,*) 'Internal file name is: '
!                  WRITE (*,*)  IFNAME
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
!     K.R. Gehringer (JPL)
!     N.J. Bachman   (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-FEB-2015 (NJB)
!
!        Based on DASRFR Version 1.0.0, 15-JUL-1992 (NJB) (WLT)
!
!-&


! $ Index_Entries
!
!     read DAS file record
!     read DAS internal file name
!
!-&

! $ Revisions
!
!     None.
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
      integer(ip), parameter :: idwlen = 8

      integer(ip), parameter :: ifnlen = 60

      integer(ip), parameter :: nwc = 1024

!
!     Parameters for positions of file record elements:
!

!
!     ID word begin and end:
!
      integer(ip), parameter :: begidw = 1

      integer(ip), parameter :: endidw = 8


!
!     Internal file name begin and end:
!
      integer(ip), parameter :: begifn = 9

      integer(ip), parameter :: endifn = 68

!
!     Reserved record count begin and end:
!
      integer(ip), parameter :: begnrr = 69

      integer(ip), parameter :: endnrr = 72

!
!     Reserved record character count begin and end:
!
      integer(ip), parameter :: begnrc = 73

      integer(ip), parameter :: endnrc = 76


!
!     Comment area record count begin and end:
!
      integer(ip), parameter :: begncr = 77

      integer(ip), parameter :: endncr = 80

!
!     Comment area character count begin and end:
!
      integer(ip), parameter :: begncc = 81

      integer(ip), parameter :: endncc = 84

!
!     Local variables
!
      character(nwc)        chrbuf
      character(idwlen)     tmpidw
      character(ifnlen)     tmpifn

      integer(ip)           ibff
      integer(ip)           iostat
      integer(ip)           natbff
      integer(ip)           unit

      logical               first

!
!     Saved variables
!
      save                  first
      save                  natbff

!
!     Initial values
!
      data                  first  / .true. /
      data                  natbff / -1     /

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     On the first pass through this routine, get the integer code of
!     the host system's native binary file format.
!
      if ( first ) then

         call zzddhnfc ( natbff )

         if ( failed () ) return

         first = .false.

      end if

!
!     Get a logical unit for this DAS file.
!
      call zzddhhlu ( handle, 'DAS', .false., unit )

!
!     Get the integer code for the file's binary format.
!
      call zzddhppf ( unit, das, ibff )

      if ( failed () ) return

      if ( ibff == natbff ) then
!
!        We're looking at a native file. Just read the file record.
!
         read ( unit, &
                rec     =  1, &
                iostat  =  iostat ) tmpidw, &
                                    tmpifn, &
                                    nresvr, &
                                    nresvc, &
                                    ncomr, &
                                    ncomc

         if ( iostat /= 0 ) then

            call setmsg ( 'Could not DAS read file record. File was ' &
            //            '#.  IOSTAT was #.'                        )
            call errfnm ( '#', unit                                  )
            call errint ( '#', iostat                                )
            call sigerr ( 'SPICE(DASFILEREADFAILED)'                 )
            return

         end if

         idword  =  tmpidw
         ifname  =  tmpifn


      else
!
!        The file is non-native.
!
!        We don't check the access mode of the file, because we're
!        not going to reject files that are open for writing.
!
!        We'll read the file record as a character string and then
!        pick it apart.
!
         read ( unit, &
                rec     =  1, &
                iostat  =  iostat ) chrbuf

         if ( iostat /= 0 ) then

            call setmsg ( 'Could not read DAS file record. File is ' &
            //            '#. IOSTAT was #. File''s BFF integer ' &
            //            'code is #.'                              )
            call errfnm ( '#', unit                                 )
            call errint ( '#', iostat                               )
            call errint ( '#', ibff                                 )
            call sigerr ( 'SPICE(DASFILEREADFAILED)'                )
            return

         end if

!
!        Set the string output arguments.
!
         idword = chrbuf( begidw : endidw )
         ifname = chrbuf( begifn : endifn )

!
!        The integer output arguments require translation.
!
         call zzxlatei ( ibff,  chrbuf( begnrr : endnrr ),  1,  nresvr )
         call zzxlatei ( ibff,  chrbuf( begnrc : endnrc ),  1,  nresvc )
         call zzxlatei ( ibff,  chrbuf( begncr : endncr ),  1,  ncomr  )
         call zzxlatei ( ibff,  chrbuf( begncc : endncc ),  1,  ncomc  )

      end if

      return
      end
