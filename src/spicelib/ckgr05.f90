!$Procedure CKGR05 ( C-kernel, get record, type 05 )

      subroutine ckgr05 ( handle, descr, recno, record )

! $ Abstract
!
!     Return a specified pointing instance from a CK type 05 segment.
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

      include 'ck05.inc'

      integer               handle
      double precision      descr   ( * )
      integer               recno
      double precision      record  ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of the file containing the segment.
!     DESCR      I   The segment descriptor.
!     RECNO      I   The number of the pointing instance to be returned.
!     RECORD     O   The pointing record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the binary CK file containing the
!              desired segment.
!
!     DESCR    is the packed descriptor of the data type 5 segment.
!
!     RECNO    is the number of the discrete pointing instance to be
!              returned from the data type 5 segment.
!
! $ Detailed_Output
!
!     RECORD   is the pointing instance indexed by RECNO in the
!              segment. The contents are as follows:
!
!                 RECORD( 1 ) = CLKOUT
!
!              CLKOUT is the encoded spacecraft clock time associated
!              with the returned pointing values.
!
!                 RECORD( 2 ) = SUBTYP
!
!              SUBTYP is the CK type 5 subtype code. This code
!              identifies the structure and meaning of the rest
!              of the record. However, all subtypes have a
!              quaternion stored in elements 3-6.
!
!                 RECORD( 3 ) = q0
!                 RECORD( 4 ) = q1
!                 RECORD( 5 ) = q2
!                 RECORD( 6 ) = q3
!
!              Subtype 1 ends here; there are no angular velocity
!              data. Angular velocity is derived by differentiating
!              Lagrange interpolating polynomials.
!
!                 RECORD(  7 ) =  ]
!                 RECORD(  8 ) =  ] --- For subtypes 0 and 2, these
!                 RECORD(  9 ) =  ]     elements contain a quaternion
!                 RECORD( 10 ) =  ]     derivative. For subtype 3,
!                                       elements 7-9 contain an
!                                       angular velocity vector;
!                                       element 10 is unassigned.
!
!                                       All subtypes except subtype
!                                       2 stop here.
!
!                 RECORD( 11 ) =  ]
!                 RECORD( 12 ) =  ] --- For subtype 2, these
!                 RECORD( 13 ) =  ]     elements contain an angular
!                                       velocity vector.
!
!
!                 RECORD( 14 ) =  ]
!                 RECORD( 15 ) =  ] --- For subtype 2, these
!                 RECORD( 16 ) =  ]     elements contain the
!                                       derivative of an angular
!                                       velocity vector.
!
!              The quantities q0 - q3 are the components of the
!              quaternion that represents the C-matrix that transforms
!              vectors from the inertial reference frame of the
!              segment to the instrument frame at time CLKOUT.
!
!              Quaternion derivatives, angular velocity, or the
!              derivative of angular velocity are returned only
!              these are supported by the segment subtype and
!              if the segment descriptor indicates that angular
!              velocity is present.
!
!              The components of the angular velocity vector are
!              specified relative to the inertial reference frame of
!              the segment.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the segment is not of data type 5, the error
!         SPICE(CKWRONGDATATYPE) is signaled.
!
!     2)  If RECNO is less than one or greater than the number of
!         records in the specified segment, the error
!         SPICE(CKNONEXISTREC) is signaled.
!
!     3)  If the specified handle does not belong to any DAF file that
!         is currently known to be open, an error is signaled by a
!         routine in the call tree of this routine.
!
!     4)  If DESCR is not a valid descriptor of a segment in the CK
!         file specified by HANDLE, the results of this routine are
!         unpredictable.
!
!     5)  If the segment subtype is not recognized, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
! $ Files
!
!     The file specified by HANDLE should be open for read or
!     write access.
!
! $ Particulars
!
!     For a detailed description of the structure of a type 5 segment,
!     see the CK required reading.
!
!     This is a utility routine that may be used to read the individual
!     pointing instances that make up a type 5 segment. It is normally
!     used in conjunction with CKNR05, which gives the number of
!     pointing instances stored in a segment.
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
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!        Bug fix: added FAILED tests after all DAFGDA calls but the
!        last.
!
!        Added explicit conversion to INTEGER for SUBTYP.
!
!-    SPICELIB Version 1.0.0, 27-AUG-2002 (NJB) (JML)
!
!-&


! $ Index_Entries
!
!     get CK type_5 record
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
!        NDC        is the number of double precision components in an
!                   unpacked C-kernel segment descriptor.
!
!        NIC        is the number of integer components in an unpacked
!                   C-kernel segment descriptor.
!
!        DTYPE      is the data type of the segment that this routine
!                   operates on.
!
!

      integer               ndc
      parameter           ( ndc    = 2 )

      integer               nic
      parameter           ( nic    = 6 )

      integer               dtype
      parameter           ( dtype  = 5 )

!
!     Local variables
!
      integer               icd    ( nic )
      integer               nrec
      integer               beg
      integer               end
      integer               addr
      integer               packsz
      integer               subtyp

      double precision      dcd    ( ndc )
      double precision      npoint



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'CKGR05' )

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
!     From the descriptor, determine
!
!       1 - Is this really a type 5 segment?
!       2 - The beginning address of the segment.
!       3 - The number of pointing instances in the segment (it's the
!           last word in the segment).
!       4 - The existence of angular velocity data, which determines how
!           big the pointing portion of the returned record will be.
!
      call dafus ( descr, ndc, nic, dcd, icd )

      if ( icd( 3 ) .ne. dtype ) then

         call setmsg ( 'Data type of the segment should be 5: Passed '// &
                       'descriptor shows type = #.'                    )
         call errint ( '#', icd( 3 )                                   )
         call sigerr ( 'SPICE(CKWRONGDATATYPE)'                        )
         call chkout ( 'CKGR05'                                        )
         return

      end if

!
!     Capture the segment's address range.
!
      beg  = icd( 5 )
      end  = icd( 6 )

!
!     Read the subtype from the segment.
!
      call dafgda ( handle, end-3, end-3, record(2) )

      if ( failed() ) then
         call chkout ( 'CKGR05' )
         return
      end if


      subtyp = nint( record(2) )

      if ( subtyp .eq. c05tp0 ) then

         packsz = c05ps0

      else if ( subtyp .eq. c05tp1 ) then

         packsz = c05ps1

      else if ( subtyp .eq. c05tp2 ) then

         packsz = c05ps2

      else if ( subtyp .eq. c05tp3 ) then

         packsz = c05ps3

      else

         call setmsg ( 'Unexpected CK type 5 subtype # found in ' // &
                       'type 5 segment.'                          )
         call errint ( '#',  subtyp                               )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         call chkout ( 'CKGR05'                                   )
         return

      end if

      call dafgda ( handle, end, end, npoint )

      if ( failed() ) then
         call chkout ( 'CKGR05' )
         return
      end if

      nrec = nint ( npoint )


!
!     If a request was made for a record which doesn't exist, then
!     signal an error and leave.
!
      if ( ( recno .lt. 1 ) .or. ( recno .gt. nrec ) ) then
         call setmsg ( 'Requested record number (#) does not exist. ' // &
                       'There are # records in the segment.'           )
         call errint ( '#', recno                                      )
         call errint ( '#', nrec                                       )
         call sigerr ( 'SPICE(CKNONEXISTREC)'                          )
         call chkout ( 'CKGR05'                                        )
         return
      end if

!
!     Get the pointing record indexed by RECNO.
!
      addr = beg + packsz * ( recno - 1 )

      call dafgda ( handle, addr, addr + packsz - 1, record( 3 ) )

      if ( failed() ) then
         call chkout ( 'CKGR05' )
         return
      end if

!
!     Next get the SCLK time.  Need to go past all of the NREC pointing
!     records (PACKSZ * NREC numbers), and then to the RECNOth SCLK
!     time.
!
      addr = beg + packsz*nrec + recno - 1

      call dafgda ( handle, addr, addr, record( 1 ) )

      call chkout ( 'CKGR05' )
      return
      end
