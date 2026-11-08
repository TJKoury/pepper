!$Procedure DLABNS ( DLA, begin new segment )

      SUBROUTINE DLABNS ( HANDLE )

! $ Abstract
!
!     Begin a new segment in a DLA file.
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
!     DLA
!
! $ Keywords
!
!     DAS
!     DLA
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dla.inc'

      INTEGER               HANDLE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of open DLA file.
!
! $ Detailed_Input
!
!     HANDLE   is the integer handle associated with the DLA file to
!              be updated. This handle is used to identify the file
!              in subsequent calls to other DLA or DAS routines.
!
!              The DLA file must be open for write access. A new DLA
!              segment is started in the indicated file. The file
!              is left open, since normally data will be written to
!              the file following a call to this routine.
!
! $ Detailed_Output
!
!     None. See the $Particulars and $Examples header sections for
!     a description of the actions performed by this routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input file handle does not refer to a DAS file that is
!         open for write access, an error is signaled by a routine
!         in the call tree of this routine.
!
!     2)  If an error occurs while reading or writing to the DLA file,
!         the error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See description of input argument HANDLE.
!
! $ Particulars
!
!     DLA files are built using the DAS low-level format; DLA files are
!     a specialized type of DAS file in which data are organized as a
!     doubly linked list of segments. Each segment's data belong to
!     contiguous components of character, double precision, and integer
!     type.
!
!     This routine supports creation of a DLA segment. DLA segments
!     are created by appending data to the DAS integer, double
!     precision, and character address spaces of a DLA file. The new
!     segment's descriptor is located immediately before the integer
!     component of the segment's data.
!
!     When a new segment is added to a DLA file, the segment is
!     inserted into the file's doubly linked segment list. If the new
!     segment is the first, the DLA file's first and last list entry
!     pointers are updated to point to the new segment; specifically,
!     these pointers point to the first integer of the new segment's
!     descriptor. The backward pointer of the new segment is set to
!     null in this case.
!
!     If the new segment is not the first, the DLA file's list end
!     pointer is updated to point to the new segment, and the forward
!     pointer of the previous segment also is updated to point to the
!     first integer of the new segment's descriptor. The backward
!     pointer of the new segment points to the first integer of the
!     previous segment's descriptor.
!
!     The normal sequence of operations required to create a DLA
!     segment is as follows:
!
!        Call DLAOPN to create a new, empty DLA file.
!
!        For each segment to be created,
!
!           Call DLABNS to begin a segment.
!
!           Use the DAS "add" and "update" routines to populate
!           the segment with data.
!
!           Call DLAENS to end the segment.
!
!        Call DASCLS to segregate and close the DLA file.
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
!              PROGRAM DLABNS_EX1
!              IMPLICIT NONE
!
!              INCLUDE 'dla.inc'
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         DLA
!              PARAMETER           ( DLA    = 'dlabns_ex1.dla' )
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
!-    SPICELIB Version 1.0.1, 14-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Modified
!        the presentation of the output in the code example to comply
!        with the maximum line length for the header comments.
!
!-    SPICELIB Version 1.0.0, 08-FEB-2017 (NJB)
!
!        Updated version info.
!
!        13-JUL-2012 (NJB)
!
!           Bug fix: deleted unused line of code.
!           Fixed some header typos.
!
!        08-OCT-2009 (NJB)
!
!           Updated header.
!
!        11-FEB-2005 (NJB)
!
!-&


! $ Index_Entries
!
!     begin new segment in DLA file
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
      INTEGER               FSTIDX
      PARAMETER           ( FSTIDX = 1 )

      INTEGER               LSTIDX
      PARAMETER           ( LSTIDX = 2 )

!
!     Local variables
!
      INTEGER               ADDR
      INTEGER               SGPTRS ( 2 )
      INTEGER               DESCR  ( DLADSZ )
      INTEGER               LASTC
      INTEGER               LASTD
      INTEGER               LASTI
      INTEGER               THIS

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'DLABNS' )

!
!     Make sure the input handle refers to a DAS file that
!     is open for write access.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DLABNS' )
         RETURN
      END IF

!
!     Look up the pointers to the first and last DLA segment
!     descriptors in the file.  If no segments are present,
!     the pointers will contain the value NULPTR.
!
      CALL DASRDI ( HANDLE, LLBIDX, LLEIDX, SGPTRS )

!
!     Find the last DAS logical addresses in use for each data type.
!
      CALL DASLLA ( HANDLE, LASTC, LASTD, LASTI )

!
!     Initialize a DLA descriptor with null values.  If this
!     is not the first segment in the file, the backward pointer
!     must be set to point to the previous descriptor.  It's
!     valid to overwrite the descriptor pointer in either case
!     due to the initialization of the file's last segment pointer
!     to NULPTR.
!
      CALL FILLI ( NULPTR, DLADSZ, DESCR )

      DESCR(BWDIDX) = SGPTRS(LSTIDX)

!
!     Set the descriptor's component base addresses now.  The
!     base addresses are *predecessors* of the first address of
!     each data type.  This choice slightly simplifies arithmetic
!     needed to express the address range occupied by a segment
!     component:  the range is
!
!        base + 1  :  base + size
!
!     For the integer address, add on the size of the descriptor
!     we're about to write.
!
      DESCR(IBSIDX) = LASTI + DLADSZ
      DESCR(DBSIDX) = LASTD
      DESCR(CBSIDX) = LASTC

!
!     Append the descriptor to the file.
!
      CALL DASADI ( HANDLE, DLADSZ, DESCR )

!
!     THIS is the pointer to the current descriptor.
!
      THIS = LASTI + 1

!
!     If this is not the first segment, the forward pointer of the
!     previous descriptor must be updated to point to this descriptor.
!
      IF ( SGPTRS(LSTIDX) .NE. NULPTR ) THEN

         ADDR = SGPTRS(LSTIDX) + FWDIDX - 1

         CALL DASUDI ( HANDLE, ADDR, ADDR, THIS )

      END IF

!
!     Update the segment list pointers in the file. The begin pointer
!     must be updated only if it's null.  The end pointer will point to
!     this segment.
!
      IF ( SGPTRS(FSTIDX) .EQ. NULPTR ) THEN

         CALL DASUDI ( HANDLE, LLBIDX, LLBIDX, THIS )

      END IF

      CALL DASUDI ( HANDLE, LLEIDX, LLEIDX, THIS )

!
!     Leave the file open.  The segment is now ready to be
!     populated with data.  The routines DASADC, DASADD, and
!     DASADI should be used to append data to the segment.
!

      CALL CHKOUT ( 'DLABNS' )
      RETURN
      END
