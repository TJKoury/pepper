!$Procedure CKGR01 ( C-kernel, get record, type 01 )

      subroutine ckgr01 ( handle, descr, recno, record )

! $ Abstract
!
!     Return a specified pointing instance from a CK type 01 segment.
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
!     RECNO      I   The number of the pointing record to be returned.
!     RECORD     O   The pointing record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the binary CK file containing the
!              desired segment. The file should have been opened
!              for read access, either by CKLPF or DAFOPR.
!
!     DESCR    is the packed descriptor of the data type 1 segment.
!
!     RECNO    is the number of the individual pointing record to be
!              returned from the data type 1 segment.
!
! $ Detailed_Output
!
!     RECORD   is the pointing record indexed by RECNO in the segment.
!              The contents are as follows:
!
!                 RECORD( 1 ) = CLKOUT
!
!                 RECORD( 2 ) = q0
!                 RECORD( 3 ) = q1
!                 RECORD( 4 ) = q2
!                 RECORD( 5 ) = q3
!
!                 RECORD( 6 ) = Av1  ]
!                 RECORD( 7 ) = Av2  |-- Returned optionally
!                 RECORD( 8 ) = Av3  ]
!
!              CLKOUT is the encoded spacecraft clock time associated
!              with the returned pointing values.
!
!              The quantities q0 - q3 represent a quaternion.
!              The quantities Av1, Av2, and Av3 represent the
!              angular velocity vector, and are returned only if the
!              segment contains angular velocity data. The
!              components of the angular velocity vector are
!              specified relative to the inertial reference
!              frame of the segment.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the segment is not of data type 1, the error
!         SPICE(CKWRONGDATATYPE) is signaled.
!
!     2)  If RECNO is less than one or greater than the number of
!         records in the specified segment, the error
!         SPICE(CKNONEXISTREC) is signaled.
!
!     3)  If the specified handle does not belong to any file that is
!         currently known to be open, an error is signaled by a routine
!         in the call tree of this routine.
!
!     4)  If DESCR is not a valid, packed descriptor of a segment in
!         the CK file specified by HANDLE, the results of this routine
!         are unpredictable.
!
! $ Files
!
!     The file specified by HANDLE should be open for read access.
!
! $ Particulars
!
!     For a detailed description of the structure of a type 1 segment,
!     see the CK required reading.
!
!     This is a utility routine that performs as follows. It finds out
!     how many records are in the segment, checks to see if the request
!     fits the bounds of the segment, and then moves directly to get
!     the requested data.
!
! $ Examples
!
!     The following code fragment prints the records of the first
!     segment in a CK file. Suppose MOC.CK is valid CK file that
!     contains segments of data type 1.
!
!           INTEGER               ICD     ( 6 )
!           INTEGER               HANDLE
!           INTEGER               NREC
!           INTEGER               I
!           DOUBLE PRECISION      DCD     ( 2 )
!           DOUBLE PRECISION      DESCR   ( 5 )
!           DOUBLE PRECISION      RECORD  ( 8 )
!           LOGICAL               FOUND
!
!     C
!     C     First load the file. (The file may also be opened by using
!     C     CKLPF.)
!     C
!           CALL DAFOPR ( 'MOC.CK', HANDLE )
!
!     C
!     C     Begin forward search.  Find first array.
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
!           IF ( ICD( 3 ) .EQ. 1 ) THEN
!
!     C
!     C        How many records does this segment contain?
!     C
!              CALL CKNR01 ( HANDLE, DESCR, NREC )
!
!              DO I = 1, NREC
!
!     C
!     C           Get the record associated with record number I.
!     C
!                 CALL CKGR01 ( HANDLE, DESCR, I, RECORD )
!                 WRITE (*,*) 'Record ', I, ':'
!                 WRITE (*,*)  RECORD
!              END DO
!
!           END IF
!
! $ Restrictions
!
!     1)  The binary CK file containing the segment whose descriptor was
!         passed to this routine must be opened for read access by
!         either CKLPF or DAFOPR.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.2, 06-MAR-1991 (JML)
!
!        A correction was made to the example program in the
!        header. The array of double precision components of
!        the descriptor ( DCD ) had originally been declared
!        as an integer.
!
!-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
!
!        The restriction that a C-kernel file must be loaded
!        was explicitly stated.
!
!-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET) (IMU)
!
!-&


! $ Index_Entries
!
!     get CK type_1 record
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!
!-    SPICELIB Version 1.0.2, 06-MAR-1991 (JML)
!
!        A correction was made to the example program in the
!        header. The array of double precision components of
!        the descriptor ( DCD ) had originally been declared
!        as an integer.
!
!-    SPICELIB Version 1.0.1, 02-NOV-1990 (JML)
!
!        1) The restriction that a C-kernel file must be loaded
!           was explicitly stated.
!        2) ROTATIONS was removed from the required reading section.
!        3) Minor changes were made to the wording of the header.
!
!
!-    Beta Version 1.1.0, 28-AUG-1990 (MJS) (JEM)
!
!        The following changes were made as a result of the
!        NAIF CK Code and Documentation Review:
!
!        1) The name of this routine was changed from CK01GR to
!           CKGR01 in order to be consistent with the SPICELIB
!           naming convention.
!        2) The declarations for the parameters QSIZ, QAVSIZ, NDC, and
!           NIC were moved from the "Declarations" section of the
!           header to the "Local parameters" section of the code below
!           the header. These parameters are not meant to modified by
!           users.
!        3) The header was corrected, improved, and updated to reflect
!           the changes.
!        4) The in-code comments were improved.
!
!-    Beta Version 1.0.0, 23-MAY-1990 (RET) (IMU)
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
!                   unpacked C-kernel segment descriptor.
!
!        NIC        is the number of integer components in an unpacked
!                   C-kernel segment descriptor.
!
!        QSIZ       is the number of double precision numbers making up
!                   the quaternion portion of a pointing record.
!
!        QAVSIZ     is the number of double precision numbers making up
!                   the quaternion and angular velocity portion of a
!                   pointing record.
!

      integer               ndc
      parameter           ( ndc    = 2 )

      integer               nic
      parameter           ( nic    = 6 )

      integer               qsiz
      parameter           ( qsiz   = 4 )

      integer               qavsiz
      parameter           ( qavsiz = 7 )

!
!     Local variables
!
      integer               icd    ( nic )
      integer               nrec
      integer               beg
      integer               addr
      integer               psiz

      double precision      dcd    ( ndc )
      double precision      n



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'CKGR01' )
      end if

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
!       1 - Is this really a type 1 segment?
!       2 - The beginning address of the segment.
!       3 - The number of records in the segment (it's the last number
!           in the segment).
!       4 - The existence of angular velocity data, which determines how
!           big the pointing portion of the returned record will be.
!
      call dafus ( descr, ndc, nic, dcd, icd )

      if ( icd( 3 ) .ne. 1 ) then
         call setmsg ( 'Data type of the segment should be 1: Passed '// &
                       'descriptor shows type = #.'                    )
         call errint ( '#', icd( 3 )                                   )
         call sigerr ( 'SPICE(CKWRONGDATATYPE)'                        )
         call chkout ( 'CKGR01'                                        )
         return
      end if

      beg  = icd( 5 )

      call dafgda ( handle, icd( 6 ), icd( 6 ), n )
      nrec = int( n )

      if ( icd( 4 ) .eq. 1 ) then
         psiz = qavsiz
      else
         psiz = qsiz
      end if

!
!     If a request was made for a record which doesn't exist, then
!     signal an error and leave.
!
      if ( recno .lt. 1 .or. recno .gt. nrec ) then
         call setmsg ( 'Requested record number (#) does not exist. ' // &
                       'There are # records in the segment.'           )
         call errint ( '#', recno                                      )
         call errint ( '#', nrec                                       )
         call sigerr ( 'SPICE(CKNONEXISTREC)'                          )
         call chkout ( 'CKGR01'                                        )
         return
      end if

!
!     Get the pointing record indexed by RECNO.
!
      addr = beg + psiz*( recno - 1 )
      call dafgda ( handle, addr, addr + ( psiz - 1 ), record( 2 ))

!
!     Next get the SCLK time.  Need to go past all of the NREC pointing
!     records (PSIZ * NREC numbers), and then to the RECNOth SCLK
!     time.
!
      addr = beg + psiz*nrec + recno - 1
      call dafgda ( handle, addr, addr, record( 1 ) )


      call chkout ( 'CKGR01' )
      return
      end
