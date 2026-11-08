!$Procedure DAFGDA ( DAF, read data from address )

      subroutine dafgda ( handle, baddr, eaddr, data )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read the double precision data bounded by two addresses within
!     a DAF.
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

      implicit none

      integer(ip)           handle
      integer(ip)           baddr
      integer(ip)           eaddr
      real(wp)              data     ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAF.
!     BADDR,
!     EADDR      I   Initial, final address within file.
!     DATA       O   Data contained between BADDR and EADDR.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a DAF.
!
!     BADDR,
!     EADDR    are the initial and final addresses of a contiguous
!              set of double precision numbers within a DAF.
!              Presumably, these make up all or part of a particular
!              array.
!
! $ Detailed_Output
!
!     DATA     are the double precision data contained between
!              the specified addresses within the specified file.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If BADDR is zero or negative, the error SPICE(DAFNEGADDR)
!         is signaled.
!
!     2)  If BADDR > EADDR, the error SPICE(DAFBEGGTEND) is signaled.
!
!     3)  If HANDLE is invalid, an error is signaled by a routine in the
!         call tree of this routine.
!
!     4)  If the range of addresses covered between BADDR and EADDR
!         includes records that do not contain strictly double
!         precision data, then the values returned in DATA are
!         undefined. See the $Restrictions section below for details.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The principal reason that DAFs are so easy to use is that
!     the data in each DAF are considered to be one long contiguous
!     set of double precision numbers. You can grab data from anywhere
!     within a DAF without knowing (or caring) about the physical
!     records in which they are stored.
!
!     This routine replaces DAFRDA as the principle mechanism for
!     reading the contents of DAF arrays.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Open a type 8 SPK for read access, retrieve the data for
!        the first segment and identify the beginning and end addresses,
!        the number of data elements within, the size of the data
!        array, and print the first two records.
!
!        Use the SPK kernel below as input type 8 SPK file for the
!        example.
!
!           mer1_ls_040128_iau2000_v1.bsp
!
!        Each segment contains only two records which provide the start
!        and end position for the MER-1 rover landing site in the
!        IAU_MARS frame. Since the landing site does not change over
!        time, it is expected that both records are equal.
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFGDA_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local constants.
!        C
!              CHARACTER*(*)         FMT
!              PARAMETER           ( FMT = '(6F10.3)' )
!
!              INTEGER               MAXDAT
!              PARAMETER           ( MAXDAT = 1000 )
!
!              INTEGER               MAXSUM
!              PARAMETER           ( MAXSUM = 125  )
!
!              INTEGER               ND
!              PARAMETER           ( ND     = 2    )
!
!              INTEGER               NI
!              PARAMETER           ( NI     = 6    )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      DAFSUM ( MAXSUM )
!              DOUBLE PRECISION      DATA   ( MAXDAT )
!              DOUBLE PRECISION      DC     ( ND )
!
!              INTEGER               BADDR
!              INTEGER               EADDR
!              INTEGER               HANDLE
!              INTEGER               IC     ( NI )
!              INTEGER               SIZE
!
!              LOGICAL               FOUND
!
!        C
!        C     Open the type 8 SPK for read access, then read the
!        C     data from the first segment.
!        C
!              CALL DAFOPR ( 'mer1_ls_040128_iau2000_v1.bsp', HANDLE )
!
!        C
!        C     Begin a forward search; find the first segment; read the
!        C     segment summary.
!        C
!              CALL DAFBFS ( HANDLE )
!              CALL DAFFNA ( FOUND  )
!              CALL DAFGS  ( DAFSUM )
!              CALL DAFUS  ( DAFSUM, ND, NI, DC, IC )
!
!        C
!        C     Retrieve the data begin and end addresses.
!        C
!              BADDR = IC(5)
!              EADDR = IC(6)
!
!              WRITE(*,'(A,I4)') 'Beginning address       : ', BADDR
!              WRITE(*,'(A,I4)') 'Ending address          : ', EADDR
!              WRITE(*,'(A,I4)') 'Number of data elements : ',
!             .                                    EADDR - BADDR + 1
!
!        C
!        C     Extract all data bounded by the begin and end addresses.
!        C
!              CALL DAFGDA ( HANDLE, BADDR, EADDR, DATA )
!
!        C
!        C     Check the data.
!        C
!              WRITE(*,'(A)') 'The first and second states '
!             .            // 'stored in the segment:'
!              WRITE(*,FMT) DATA(1),  DATA(2),  DATA(3),
!             .             DATA(4),  DATA(5),  DATA(6)
!              WRITE(*,FMT) DATA(7),  DATA(8),  DATA(9),
!             .             DATA(10), DATA(11), DATA(12)
!
!        C
!        C     Safely close the file
!        C
!              CALL DAFCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Beginning address       :  897
!        Ending address          :  912
!        Number of data elements :   16
!        The first and second states stored in the segment:
!          3376.422  -326.649  -115.392     0.000     0.000     0.000
!          3376.422  -326.649  -115.392     0.000     0.000     0.000
!
!
! $ Restrictions
!
!     1)  There are several types of records in a DAF. This routine
!         is only to be used to read double precision data bounded
!         between two DAF addresses. The range of addresses input
!         may not cross data and summary record boundaries.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
!
!        Changed the input argument names BEGIN and END to BADDR to
!        EADDR for consistency with other routines.
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added
!        complete code example. Removed unnecessary $Revisions section.
!
!-    SPICELIB Version 1.0.0, 16-NOV-2001 (FST)
!
!-&


! $ Index_Entries
!
!     read data from DAF address
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           bsize
      parameter           ( bsize = 128 )

      integer(ip)           begr
      integer(ip)           begw
      integer(ip)           endr
      integer(ip)           endw

      integer(ip)           recno
      integer(ip)           first
      integer(ip)           last
      integer(ip)           next

      logical               found

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Bad addresses?
!
      if ( baddr <= 0 ) then
         call setmsg ( 'Negative value for BADDR address: #' )
         call errint ( '#', baddr )
         call sigerr ( 'SPICE(DAFNEGADDR)' )
         return

      else if ( baddr > eaddr ) then
         call setmsg ( 'Beginning address (#) greater than ending '   // &
                       'address (#).' )
         call errint ( '#', baddr )
         call errint ( '#', eaddr   )
         call sigerr ( 'SPICE(DAFBEGGTEND)' )
         return
      end if

!
!     Convert raw addresses to record/word representations.
!
      call dafarw ( baddr, begr, begw )
      call dafarw ( eaddr, endr, endw )

!
!     Get as many records as needed. Return the last part of the
!     first record, the first part of the last record, and all of
!     every record in between. Any record not found is assumed to
!     be filled with zeros.
!
      next = 1

      do recno = begr, endr

         if ( begr == endr ) then
            first = begw
            last  = endw

         else if ( recno == begr ) then
            first = begw
            last  = bsize

         else if ( recno == endr ) then
            first = 1
            last  = endw

         else
            first = 1
            last  = bsize
         end if

         call dafgdr ( handle, recno, first, last, data( next ), found )

         if ( .not. found ) then
            call cleard ( last-first+1, data( next ) )
         end if

         next = next + ( last-first+1 )

      end do

      return
      end
