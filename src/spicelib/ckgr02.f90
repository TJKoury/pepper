!$Procedure CKGR02 ( C-kernel, get record, type 02 )

      subroutine ckgr02 ( handle, descr, recno, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return a specified pointing instance from a CK type 02 segment.
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
      integer(ip)           recno
      real(wp)              record  ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   The handle of the CK file containing the segment.
!     DESCR      I   The segment descriptor.
!     RECNO      I   The number of the pointing record to be returned.
!     RECORD     O   The pointing record.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the binary CK file containing the
!              desired segment. The file should have been opened
!              for read or write access, by CKLPF, DAFOPR or DAFOPW.
!
!     DESCR    is the packed descriptor of the data type 2 CK segment.
!
!     RECNO    is the number of the individual pointing record to be
!              returned from the data type 2 segment.
!
! $ Detailed_Output
!
!     RECORD   is the pointing record indexed by RECNO in the segment.
!              The contents are as follows:
!
!                 RECORD( 1  ) = start SCLK time of interval
!                 RECORD( 2  ) = end SCLK time of interval
!                 RECORD( 3  ) = TDB seconds per tick rate
!
!                 RECORD( 4  ) = Q1
!                 RECORD( 5  ) = Q2
!                 RECORD( 6  ) = Q3
!                 RECORD( 7  ) = Q4
!
!                 RECORD( 8  ) = AV1
!                 RECORD( 9  ) = AV2
!                 RECORD( 10 ) = AV3
!
!
!              See the section on data type 2 in the CK Required
!              Reading for a complete description on how pointing
!              is obtained from a type 2 record.
!
!              Note that the RECORD returned by this routine is slightly
!              different from that returned by the SPICELIB routine
!              CKR02. The second element of the record returned by that
!              routine contains the SCLK time at which pointing was
!              requested, whereas this routine returns the SCLK time of
!              the right endpoint of the interval for which the constant
!              angular velocity model is valid.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the segment is not of data type 2, the error
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
!     4)  If DESCR is not a valid descriptor of a segment in the CK
!         file specified by HANDLE, the results of this routine are
!         unpredictable.
!
! $ Files
!
!     The CK file specified by HANDLE should be open for read or write
!     access.
!
! $ Particulars
!
!     For a detailed description of the structure of a type 2 segment,
!     see the CK Required Reading.
!
!     This is a utility routine that may be used to read the individual
!     pointing records that make up a data type 2 segment. It is
!     normally used in combination with CKNR02, which gives the number
!     of pointing instances stored in a segment.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following code example extracts the start and end SCLK
!        time, seconds per tick rate, platform's +Z axis direction,
!        and angular velocity vector for each pointing instance in
!        the first segment in a CK file that contains segments of data
!        type 2.
!
!        Use the CK kernel below, available in the Viking Orbiter PDS
!        archives, as input for the code example.
!
!           vo2_swu_ck2.bc
!
!        Example code begins here.
!
!
!              PROGRAM CKGR02_EX1
!              IMPLICIT NONE
!
!              DOUBLE PRECISION      AV      ( 3 )
!              DOUBLE PRECISION      CMAT    ( 3, 3 )
!              DOUBLE PRECISION      DCD     ( 2  )
!              DOUBLE PRECISION      DESCR   ( 5  )
!              DOUBLE PRECISION      QUAT    ( 4 )
!              DOUBLE PRECISION      RECORD  ( 10 )
!              DOUBLE PRECISION      SCLKE
!              DOUBLE PRECISION      SCLKR
!              DOUBLE PRECISION      SCLKS
!              DOUBLE PRECISION      Z       ( 3 )
!
!              INTEGER               ICD     ( 6 )
!              INTEGER               HANDLE
!              INTEGER               NREC
!              INTEGER               I
!
!              LOGICAL               FOUND
!
!        C
!        C     First load the file. (The file may also be opened by
!        C     using CKLPF).
!        C
!              CALL DAFOPR ( 'vo2_swu_ck2.bc', HANDLE )
!
!        C
!        C     Begin forward search.  Find the first array.
!        C
!              CALL DAFBFS ( HANDLE )
!              CALL DAFFNA ( FOUND  )
!
!        C
!        C     Get segment descriptor.
!        C
!              CALL DAFGS ( DESCR )
!
!        C
!        C     Unpack the segment descriptor into its double precision
!        C     and integer components.
!        C
!              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!
!        C
!        C     The data type for a segment is located in the third
!        C     integer component of the descriptor.
!        C
!              IF ( ICD( 3 ) .EQ. 2 ) THEN
!
!        C
!        C        How many records does this segment contain?
!        C
!                 CALL CKNR02 ( HANDLE, DESCR, NREC )
!
!                 DO I = 1, NREC
!
!        C
!        C           Get the Ith record in the segment.
!        C
!                    CALL CKGR02 ( HANDLE, DESCR, I, RECORD )
!
!        C
!        C           Unpack RECORD into the start and end time, rate in
!        C           TDB seconds/tick, quaternion, and av.
!        C
!                    SCLKS = RECORD ( 1 )
!                    SCLKE = RECORD ( 2 )
!                    SCLKR = RECORD ( 3 )
!
!                    CALL MOVED ( RECORD(4), 4, QUAT )
!                    CALL MOVED ( RECORD(8), 3, AV   )
!
!        C
!        C           The +Z axis direction is the third row of the
!        C           C-matrix.
!        C
!                    CALL Q2M ( QUAT, CMAT )
!
!                    Z(1) = CMAT(3,1)
!                    Z(2) = CMAT(3,2)
!                    Z(3) = CMAT(3,3)
!
!        C
!        C           Write out the results.
!        C
!                    WRITE (*,'(A,I2)') 'Record: ', I
!                    WRITE (*,'(A,F21.6)') '   Start encoded SCLK:',
!             .                              SCLKS
!                    WRITE (*,'(A,F21.6)') '   End encoded SCLK  :',
!             .                              SCLKE
!                    WRITE (*,'(A,F21.6)') '   TDB Seconds/tick  :',
!             .                              SCLKR
!                    WRITE (*,'(A,3F13.8)') '   +Z axis           :',
!             .                              Z
!                    WRITE (*,'(A,3F13.8)') '   Angular velocity  :',
!             .                              AV
!                    WRITE (*,*)
!
!                 END DO
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Record:  1
!           Start encoded SCLK:   32380393707.000015
!           End encoded SCLK  :   32380395707.000015
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.62277152  -0.29420673   0.72498141
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record:  2
!           Start encoded SCLK:   32380402605.999947
!           End encoded SCLK  :   32380404605.999947
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.62172600  -0.27894910   0.73187716
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record:  3
!           Start encoded SCLK:   32380412542.000053
!           End encoded SCLK  :   32380414542.000053
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.62183610  -0.26307233   0.73764003
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record:  4
!           Start encoded SCLK:   32827264875.000000
!           End encoded SCLK  :   32827266875.000000
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.82984105  -0.44796078   0.33270853
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record:  5
!           Start encoded SCLK:   32827403805.999992
!           End encoded SCLK  :   32827405805.999992
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.80812500  -0.44911178   0.38109395
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record:  6
!           Start encoded SCLK:   32827412705.000042
!           End encoded SCLK  :   32827414705.000042
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.81120505  -0.43593555   0.38975193
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record:  7
!           Start encoded SCLK:   32827417284.000038
!           End encoded SCLK  :   32827419284.000038
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.81313834  -0.42861613   0.39382008
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record:  8
!           Start encoded SCLK:   33793314593.000053
!           End encoded SCLK  :   33793316593.000053
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.79860617  -0.37840751   0.46801275
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record:  9
!           Start encoded SCLK:   33793332478.000046
!           End encoded SCLK  :   33793334478.000046
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.77861783  -0.39171670   0.49021658
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record: 10
!           Start encoded SCLK:   33793341463.000061
!           End encoded SCLK  :   33793343463.000061
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.76852381  -0.39797437   0.50098659
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record: 11
!           Start encoded SCLK:   33793350363.000034
!           End encoded SCLK  :   33793352363.000034
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.75779934  -0.40484364   0.51170478
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record: 12
!           Start encoded SCLK:   33984028250.000000
!           End encoded SCLK  :   33984030250.000000
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.77099184  -0.39926339   0.49614546
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record: 13
!           Start encoded SCLK:   33984046134.999992
!           End encoded SCLK  :   33984048134.999992
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.75024440  -0.41218993   0.51694564
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record: 14
!           Start encoded SCLK:   33984055121.000053
!           End encoded SCLK  :   33984057121.000053
!           TDB Seconds/tick  :             0.001000
!           +Z axis           :   0.73947359  -0.41886863   0.52699894
!           Angular velocity  :   0.00000000   0.00000000   0.00000000
!
!        Record: 15
!           Start encoded SCLK:   33984220835.999966
!
!        [...]
!
!
!        Warning: incomplete output. Only 100 out of 875 lines have been
!        provided.
!
!
! $ Restrictions
!
!     1)  The binary CK file containing the segment whose descriptor was
!         passed to this routine must be opened for read or write access
!         by CKLPF, DAFOPR or DAFOPW.
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 26-OCT-2021 (NJB) (JDR)
!
!        Edited the header to comply with NAIF standard. Created
!        complete code example from existing code fragment.
!
!        Improved header documentation.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
!
!-&


! $ Index_Entries
!
!     get CK type_2 record
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
!        PSIZ       is the number of double precision numbers making up
!                   the quaternion, angular velocity, and seconds per
!                   tick rate portion of a pointing record.
!
!        DTYPE      is the data type.
!

      integer(ip)           ndc
      parameter           ( ndc    = 2 )

      integer(ip)           nic
      parameter           ( nic    = 6 )

      integer(ip)           psiz
      parameter           ( psiz   = 8 )

      integer(ip)           dtype
      parameter           ( dtype  = 2 )

!
!     Local variables
!
      integer(ip)           icd    ( nic )
      integer(ip)           nrec
      integer(ip)           beg
      integer(ip)           addr

      real(wp)              dcd    ( ndc  )
      real(wp)              prec   ( psiz )



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'CKGR02' )
      end if


!
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

      call dafus ( descr, ndc, nic, dcd, icd )

      if ( icd( 3 ) /= dtype ) then
         call setmsg ( 'Data type of the segment should be 2: Passed '// &
                       'descriptor shows type = #.'                    )
         call errint ( '#', icd( 3 )                                   )
         call sigerr ( 'SPICE(CKWRONGDATATYPE)'                        )
         call chkout ( 'CKGR02'                                        )
         return
      end if


!
!     Find out how many pointing instances there are in the segment.
!
      call cknr02 ( handle, descr, nrec )

!
!     If a request was made for a record which doesn't exist, then
!     signal an error and leave.
!
      if ( ( recno < 1 ) .or. ( recno > nrec ) ) then
         call setmsg ( 'Requested record number (#) does not exist. ' // &
                       'There are # records in the segment.'           )
         call errint ( '#', recno                                      )
         call errint ( '#', nrec                                       )
         call sigerr ( 'SPICE(CKNONEXISTREC)'                          )
         call chkout ( 'CKGR02'                                        )
         return
      end if

!
!     The address of the first double precision number in the array
!     is stored in the fifth integer component of the descriptor.
!
      beg  = icd( 5 )


!
!     Get the pointing record indexed by RECNO.
!
      addr = beg + psiz * ( recno - 1 )

      call dafgda ( handle, addr, addr + ( psiz - 1 ), prec )

      record ( 3 ) = prec ( psiz )

      call moved ( prec, 7, record ( 4 ) )

!
!     Next get the interval start time.  Need to go past all of the
!     NREC pointing records (PSIZ * NREC numbers), and then to the
!     RECNOth SCLK start time.
!
      addr = beg  +  psiz * nrec  +  recno  -  1

      call dafgda ( handle, addr, addr, record( 1 ) )

!
!     Next get the interval stop time.  Need to go past all of the
!     NREC pointing records and start times ( (PSIZ+1)*NREC numbers ),
!     and then to the RECNOth SCLK stop time.
!
      addr = beg  +  ( psiz + 1 ) * nrec  +  recno  -  1

      call dafgda ( handle, addr, addr, record( 2 ) )

      call chkout ( 'CKGR02' )
      return
      end
