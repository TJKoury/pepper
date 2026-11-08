!$Procedure DLAOPN ( DLA, open new file )

      subroutine dlaopn ( fname, ftype, ifname, ncomch, handle )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Open a new DLA file and set the file type.
!
! $ Required_Reading
!
!     DAS
!     DLA
!
! $ Keywords
!
!     DAS
!     DLA
!     FILES
!
! $ Declarations

      implicit none

      include 'dla.inc'

      character(*)          fname
      character(*)          ftype
      character(*)          ifname
      integer(ip)           ncomch
      integer(ip)           handle

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FNAME      I   Name of a DLA file to be opened.
!     FTYPE      I   Mnemonic code for type of data in the DLA file.
!     IFNAME     I   Internal file name.
!     NCOMCH     I   Number of comment characters to allocate.
!     HANDLE     O   Handle assigned to the opened DLA file.
!
! $ Detailed_Input
!
!     FNAME    is the name of a new DLA file to be created. The file
!              will be left opened for write access.
!
!     FTYPE    is a code for type of data placed into a DLA file. The
!              non-blank part of FTYPE is used as the "file type"
!              portion of the ID word in the DLA file.
!
!              The first nonblank character and the three, or fewer,
!              characters immediately following it, giving four
!              characters, are used to represent the type of the data
!              placed in the DLA file. This is provided as a convenience
!              for higher level software. It is an error if this string
!              is blank. Also, the file type may not contain any
!              nonprinting characters. When written to the DLA file, the
!              value for the type IS case sensitive.
!
!              NAIF has reserved for its own use file types consisting
!              of the upper case letters (A-Z) and the digits 0-9. NAIF
!              recommends lower case or mixed case file types be used by
!              all others in order to avoid any conflicts with NAIF file
!              types.
!
!     IFNAME   is the internal file name for the new file. The name may
!              contain as many as 60 characters. This name should
!              uniquely identify the file.
!
!     NCOMCH   is the number of comment characters to allocate.
!
!              NCOMCH is used to establish the number of comment records
!              that will be allocated to the new DLA file. The number of
!              comment records allocated is the minimum required to
!              store the specified number of comment characters.
!
!              Allocating comment records at file creation time may
!              reduce the likelihood of having to expand the
!              comment area later.
!
! $ Detailed_Output
!
!     HANDLE   is the file handle associated with the file. This handle
!              is used to identify the file in subsequent calls to other
!              DLA routines.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input filename is blank, an error is signaled by a
!         routine in the call tree of this routine. No file will be
!         created.
!
!     2)  If the specified file cannot be opened without exceeding the
!         maximum allowed number of open DAS files, an error is signaled
!         by a routine in the call tree of this routine. No file will be
!         created.
!
!     3)  If the file cannot be opened properly, an error is signaled by
!         a routine in the call tree of this routine. No file will be
!         created.
!
!     4)  If the initial records in the file cannot be written, an error
!         is signaled by a routine in the call tree of this routine. No
!         file will be created.
!
!     5)  If no logical units are available, an error is signaled by a
!         routine in the call tree of this routine. No file will be
!         created.
!
!     6)  If the file type is blank, an error is signaled by a routine
!         in the call tree of this routine. No file will be created.
!
!     7)  If the file type contains nonprinting characters, decimal 0-31
!         and 127-255, an error is signaled by a routine in the call
!         tree of this routine. No file will be created.
!
!     8)  If the number of comment characters allocated to be allocated,
!         NCOMCH, is negative, the error SPICE(BADRECORDCOUNT) is
!         signaled. No file will be created.
!
! $ Files
!
!     See argument FNAME.
!
! $ Particulars
!
!     DLA files are built using the DAS low-level format; DLA files are
!     a specialized type of DAS file in which data are organized as a
!     doubly linked list of segments. Each segment's data belong to
!     contiguous components of character, double precision, and integer
!     type.
!
!     This routine creates a new DLA file and sets the type of the
!     file to the mnemonic code passed to it.
!
!     DLA files created by this routine have initialized file records.
!     The ID word in a DLA file record has the form
!
!        DAS/xxxx
!
!     where the characters following the slash are supplied by the
!     caller of this routine.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a DLA file containing one segment; the segment
!        contains character, double precision, and integer data.
!        After writing and closing the file, open the file for
!        read access; dump the data to standard output.
!
!
!        Example code begins here.
!
!
!              PROGRAM DLAOPN_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'dla.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         DLA
!              PARAMETER           ( DLA    = 'dlaopn_ex1.dla' )
!
!              INTEGER               IFNLEN
!              PARAMETER           ( IFNLEN =  60 )
!
!              INTEGER               LNSIZE
!              PARAMETER           ( LNSIZE =  61 )
!
!              INTEGER               MAXC
!              PARAMETER           ( MAXC   =  5 )
!
!              INTEGER               MAXD
!              PARAMETER           ( MAXD   =  50 )
!
!              INTEGER               MAXI
!              PARAMETER           ( MAXI   =  100 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(LNSIZE)    CVALS   ( MAXC )
!              CHARACTER*(LNSIZE)    CVALS2  ( MAXC )
!              CHARACTER*(IFNLEN)    IFNAME
!
!              DOUBLE PRECISION      DVALS   ( MAXD )
!              DOUBLE PRECISION      DVALS2  ( MAXD )
!
!              INTEGER               BASE
!              INTEGER               DESCR   ( DLADSZ )
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               IVALS   ( MAXI )
!              INTEGER               IVALS2  ( MAXI )
!              INTEGER               J
!              INTEGER               K
!              INTEGER               N
!              INTEGER               NCOMCH
!
!              LOGICAL               FOUND
!
!        C
!        C     Set the internal file name.  Don't reserve characters in
!        C     the DAS comment area.
!        C
!              IFNAME = 'Example DLA file for testing'
!              NCOMCH = 0
!
!        C
!        C     Open a new DLA file.
!        C
!              CALL DLAOPN ( DLA, 'DLA', IFNAME, NCOMCH, HANDLE )
!
!        C
!        C     Begin a new segment.
!        C
!              CALL DLABNS ( HANDLE )
!
!        C
!        C     Add character data to the segment.
!        C
!              DO I = 1, MAXC
!
!                 DO J = 1, LNSIZE
!
!                    K = MOD( J+I-1, 10 )
!
!                    CALL INTSTR ( K,  CVALS(I)(J:J) )
!
!                 END DO
!
!              END DO
!
!              CALL DASADC ( HANDLE, MAXC*LNSIZE, 1, LNSIZE, CVALS )
!
!        C
!        C     Add integer and double precision data to the segment.
!        C
!              DO I = 1, MAXI
!                 IVALS(I) = I
!              END DO
!
!              CALL DASADI ( HANDLE, MAXI, IVALS )
!
!              DO I = 1, MAXD
!                 DVALS(I) = I
!              END DO
!
!              CALL DASADD ( HANDLE, MAXD, DVALS )
!
!        C
!        C     End the segment.
!        C
!              CALL DLAENS ( HANDLE )
!
!        C
!        C     Close the file.  The routine DASCLS flushes the DAS
!        C     buffers and segregates the file before closing it.
!        C
!              CALL DASCLS ( HANDLE )
!
!        C
!        C     Now read the file and check the data.
!        C
!              CALL DASOPR ( DLA, HANDLE )
!
!        C
!        C     Obtain the segment descriptor for the sole segment
!        C     in the file. We need not check the found flag
!        C     in this case because we know there is one segment
!        C     in the file.
!        C
!              CALL DLABFS ( HANDLE, DESCR, FOUND )
!
!        C
!        C     Fetch character data from the segment.  Obtain the
!        C     base address of the character data and the
!        C     character count from the descriptor.
!        C
!              BASE = DESCR(CBSIDX)
!              N    = DESCR(CSZIDX)
!
!              CALL DASRDC ( HANDLE, BASE+1, BASE+N, 1, LNSIZE, CVALS2 )
!
!        C
!        C     Display the character data.
!        C
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Character array:'
!
!              DO I = 1, N/LNSIZE
!                 WRITE (*,*) CVALS2(I)
!              END DO
!
!        C
!        C     Fetch and display the integer and double precision data.
!        C
!              BASE = DESCR(IBSIDX)
!              N    = DESCR(ISZIDX)
!
!              CALL DASRDI( HANDLE, BASE+1, BASE+N, IVALS2 )
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Integer array:'
!              DO I = 1, N/10
!                 WRITE (*,'(10I6)') (IVALS2((I-1)*10 + J), J=1, 10)
!              END DO
!
!              BASE = DESCR(DBSIDX)
!              N    = DESCR(DSZIDX)
!
!              CALL DASRDD( HANDLE, BASE+1, BASE+N, DVALS2 )
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Double precision array:'
!              DO I = 1, N/10
!                 WRITE (*,'(10F6.1)') (DVALS2((I-1)*10 + J), J=1, 10)
!              END DO
!
!        C
!        C     Close the file.  This step is unnecessary in this
!        C     program, but is a good practice in general
!        C     because closing the file frees resources.
!        C
!              CALL DASCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Character array:
!         1234567890123456789012345678901234567890123456789012345678901
!         2345678901234567890123456789012345678901234567890123456789012
!         3456789012345678901234567890123456789012345678901234567890123
!         4567890123456789012345678901234567890123456789012345678901234
!         5678901234567890123456789012345678901234567890123456789012345
!
!         Integer array:
!             1     2     3     4     5     6     7     8     9    10
!            11    12    13    14    15    16    17    18    19    20
!            21    22    23    24    25    26    27    28    29    30
!            31    32    33    34    35    36    37    38    39    40
!            41    42    43    44    45    46    47    48    49    50
!            51    52    53    54    55    56    57    58    59    60
!            61    62    63    64    65    66    67    68    69    70
!            71    72    73    74    75    76    77    78    79    80
!            81    82    83    84    85    86    87    88    89    90
!            91    92    93    94    95    96    97    98    99   100
!
!         Double precision array:
!           1.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0  10.0
!          11.0  12.0  13.0  14.0  15.0  16.0  17.0  18.0  19.0  20.0
!          21.0  22.0  23.0  24.0  25.0  26.0  27.0  28.0  29.0  30.0
!          31.0  32.0  33.0  34.0  35.0  36.0  37.0  38.0  39.0  40.0
!          41.0  42.0  43.0  44.0  45.0  46.0  47.0  48.0  49.0  50.0
!
!
!        Note that after run completion, a new DLA file exists in the
!        output directory.
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
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 14-SEP-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!        Updated the header to describe the usage of input argument
!        NCOMCH instead of the previously documented NCOMR.
!
!        Added complete code example based on that provided for DLABNS
!        and DLAENS.
!
!        Replaced sequence of asterisks with string 'xxxx'
!        in the comment line illustrating the DAS ID word syntax.
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        01-APR-2016 (NJB)
!
!           Changed short error message for invalid comment
!           count. Corrected reference to "DASCLU" in comments.
!
!        08-OCT-2009 (NJB)
!
!           Updated header.
!
!        09-FEB-2005 (NJB) (KRG)
!
!-&


! $ Index_Entries
!
!     open a new DLA file
!     open a new DLA file with write access
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!

!
!     Local variables
!
      integer(ip)           ncomr


      if ( return() ) return


!
!     Compute the number of comment records required.
!
      if ( ncomch > 0 ) then

         ncomr = ( (ncomch-1) / nchrec )  +  1

      else if ( ncomch == 0 ) then

         ncomr = 0

      else

         call setmsg ( 'Requested number of comment characters ' &
         //            'must be non-negative but was #.'         )
         call errint ( '#',  ncomch                              )
         call sigerr ( 'SPICE(BADRECORDCOUNT)'                   )
         return

      end if

!
!     Let the DAS "open new" routine do the work.
!
      call dasonw ( fname, ftype, ifname, ncomr, handle )

!
!     Write the format version.
!
      call dasadi ( handle, 1, fmtver )

!
!     Initialize the forward and backward segment list pointers.
!
      call dasadi ( handle, 1, nulptr )
      call dasadi ( handle, 1, nulptr )

!
!     We leave the file open, since further writes to the file
!     should occur next.  The file will eventually be closed
!     by a call to DASCLS or DASLLC, if all goes well.
!
      return
      end
