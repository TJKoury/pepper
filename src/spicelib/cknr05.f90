!$Procedure CKNR05 ( C-kernel, number of records, type 05 )

      subroutine cknr05 ( handle, descr, nrec )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the number of pointing instances in a CK type 05 segment.
!     The segment is identified by a CK file handle and segment
!     descriptor.
!
! $ Required_Reading
!
!     CK
!     DAF
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      implicit none

      integer(ip)           handle
      real(wp)              descr   ( * )
      integer(ip)           nrec

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of the file containing the segment.
!     DESCR      I   The descriptor of the type 5 segment.
!     NREC       O   The number of pointing instances in the segment.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the binary CK file containing the
!              segment.
!
!     DESCR    is the packed descriptor of a data type 5 segment.
!
! $ Detailed_Output
!
!     NREC     is the number of pointing instances in the type 5
!              segment.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the segment indicated by DESCR is not a type 5 segment,
!         the error SPICE(CKWRONGDATATYPE) is signaled.
!
!     2)  If the specified handle does not belong to any DAF file that
!         is currently known to be open, an error is signaled by a
!         routine in the call tree of this routine.
!
!     3)  If DESCR is not a valid descriptor of a segment in the CK
!         file specified by HANDLE, the results of this routine are
!         unpredictable.
!
! $ Files
!
!     The file specified by HANDLE should be open for read or
!     write access.
!
! $ Particulars
!
!     For a complete description of the internal structure of a type 5
!     segment, see the CK required reading.
!
!     This routine returns the number of discrete pointing instances
!     contained in the specified segment. It is normally used in
!     conjunction with CKGR05 which returns the Ith pointing instance
!     in the segment.
!
! $ Examples
!
!     Suppose that MOC.BC is a CK file that contains segments of
!     data type 5. Then the following code fragment extracts the
!     SCLK time and boresight vector for each pointing instance
!     in the first segment in the file.
!
!
!           INTEGER               ICD     ( 6 )
!           INTEGER               HANDLE
!           INTEGER               NREC
!           INTEGER               I
!
!           DOUBLE PRECISION      DCD     ( 2 )
!           DOUBLE PRECISION      DESCR   ( 5 )
!           DOUBLE PRECISION      RECORD  ( 16 )
!           DOUBLE PRECISION      QUAT    ( 4 )
!           DOUBLE PRECISION      BORE    ( 3 )
!           DOUBLE PRECISION      CMAT    ( 3, 3 )
!           DOUBLE PRECISION      SCLKDP
!
!           LOGICAL               FOUND
!
!     C
!     C     First load the file. (The file may also be opened by using
!     C     CKLPF.)
!     C
!           CALL DAFOPR ( 'MOC.BC', HANDLE )
!
!     C
!     C     Begin forward search. Find the first array.
!     C
!           CALL DAFBFS ( HANDLE )
!           CALL DAFFNA ( FOUND  )
!
!     C
!     C     Get segment descriptor.
!     C
!           CALL DAFGS ( DESCR )
!
!     C
!     C     Unpack the segment descriptor into its double precision
!     C     and integer components.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!
!     C
!     C     The data type for a segment is located in the third integer
!     C     component of the descriptor.
!     C
!           IF ( ICD( 3 ) .EQ. 5 ) THEN
!     C
!     C        How many records does this segment contain?
!     C
!              CALL CKNR05 ( HANDLE, DESCR, NREC )
!
!              DO I = 1, NREC
!     C
!     C           Get the Ith pointing instance in the segment.
!     C
!                 CALL CKGR05 ( HANDLE, DESCR, I, RECORD )
!
!     C
!     C           Unpack from RECORD the time tag and quaternion.
!     C           The locations of these items in the record are
!     C           independent of the subtype.
!     C
!                 SCLKDP = RECORD ( 1 )
!
!                 CALL MOVED ( RECORD(3), 4, QUAT )
!
!     C
!     C           The boresight vector is the third row of the C-matrix.
!     C
!                 CALL Q2M ( QUAT, CMAT )
!
!                 BORE(1) = CMAT(3,1)
!                 BORE(2) = CMAT(3,2)
!                 BORE(3) = CMAT(3,3)
!     C
!     C           Write out the results.
!     C
!                 WRITE (*,*) 'Record: ', I
!                 WRITE (*,*)
!                 WRITE (*,*) 'SCLK time = ', SCLKDP
!                 WRITE (*,*)
!                 WRITE (*,*) 'boresight: ', BORE
!
!              END DO
!
!           END IF
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
!     J.M. Lynch         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB) (JML)
!
!-&


! $ Index_Entries
!
!     number of CK type_5 records
!
!-&


!
!     SPICELIB functions
!
      logical               return


!
!     Local parameters
!
!        NDC        is the number of double precision components in an
!                   unpacked C-kernel descriptor.
!
!        NIC        is the number of integer components in an unpacked
!                   C-kernel descriptor.
!
!        DTYPE      is the data type of the segment that this routine
!                   operates on.
!

      integer(ip)           ndc
      parameter           ( ndc = 2 )

      integer(ip)           nic
      parameter           ( nic = 6 )

      integer(ip)           dtype
      parameter           ( dtype = 5 )

!
!     Local variables
!
      integer(ip)           icd    ( nic )

      real(wp)              dcd    ( ndc )
      real(wp)              npoint




!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'CKNR05' )
      end if


!
!     The number of discrete pointing instances contained in a data
!     type 5 segment is stored in the last double precision word of
!     the segment.  Since the address of the last word is stored in
!     the sixth integer component of the segment descriptor, it is
!     a trivial matter to extract the count.
!
!     The unpacked descriptor contains the following information
!     about the segment:
!
!        DCD(1)  Initial encoded SCLK
!        DCD(2)  Final encoded SCLK
!        ICD(1)  Instrument
!        ICD(2)  Inertial reference frame
!        ICD(3)  Data type
!        ICD(4)  Angular velocity flag
!        ICD(5)  Initial address of segment data
!        ICD(6)  Final address of segment data
!
!
      call dafus ( descr, ndc, nic, dcd, icd )

!
!     If this segment is not of data type 5, then signal an error.
!

      if ( icd( 3 ) /= dtype ) then
         call setmsg ( 'Data type of the segment should be 5: Passed '// &
                       'descriptor shows type = #.'                   )
         call errint ( '#', icd ( 3 )                                 )
         call sigerr ( 'SPICE(CKWRONGDATATYPE)'                       )
         call chkout ( 'CKNR05'                                       )
         return
      end if

!
!     The number of records is the final word in the segment.
!
      call dafgda ( handle, icd(6), icd(6), npoint )

      nrec = nint ( npoint )

      call chkout ( 'CKNR05' )
      return
      end
