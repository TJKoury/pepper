!$Procedure CKMETA ( CK ID to associated SCLK )

      subroutine ckmeta ( ckid,  meta, idcode )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return (depending upon the user's request) the ID code of either
!     the spacecraft or spacecraft clock associated with a C-Kernel ID
!     code.
!
! $ Required_Reading
!
!     CK
!     FRAMES
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      include               'zzctr.inc'

      integer(ip)           ckid
      character(*)          meta
      integer(ip)           idcode

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     CKID       I   The ID code for some C kernel object.
!     META       I   The kind of meta data requested 'SPK' or 'SCLK'
!     IDCODE     O   The requested SCLK or spacecraft ID code.
!
! $ Detailed_Input
!
!     CKID     is the ID code for some object whose attitude
!              and possibly angular velocity are stored in
!              some C-kernel.
!
!     META     is a character string that indicates which piece
!              of meta data to fetch. Acceptable values are
!              'SCLK' and 'SPK'. The routine is case insensitive.
!              Leading and trailing blanks are insignificant.
!              However, blanks between characters are regarded
!              as being significant.
!
! $ Detailed_Output
!
!     IDCODE   if META is 'SCLK' then the value returned in IDCODE
!              is the ID code of the spacecraft clock used for
!              converting ET to TICKS and TICKS to ET for the
!              C-kernel used to represent the attitude of the
!              object with ID code CKID.
!
!              If META is 'SPK' then the value returned in IDCODE is the
!              ID code of the spacecraft on which the platform indicated
!              by CKID is mounted.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the variable META is not recognized to be one of the
!         inputs 'SPK' or 'SCLK', the error SPICE(UNKNOWNCKMETA)
!         is signaled.
!
!     2)  If CKID is greater than -1000, the associated SCLK and SPK
!         IDs must be in the kernel pool. If they are not present
!         a value of zero is returned for the requested item. Zero
!         is never the valid ID of a spacecraft clock.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a utility routine for mapping C-kernels to associated
!     spacecraft clocks.
!
!     An association of an SCLK ID and spacecraft ID with a CK frame
!     class ID may be made by placing in a text kernel the kernel
!     variable assignments
!
!        CK_<ck_frame_class_ID_code>_SCLK = <ID code of SCLK>
!        CK_<ck_frame_class_ID_code>_SPK  = <SPK ID code>
!
!     See the Frames Required Reading section on CK frames.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose you would like to look up the attitude of an object
!        in a C-kernel but have ET and seconds as your input time and
!        tolerance.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: ckmeta_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name              Contents
!              --------------------   -----------------------
!              cas00071.tsc           CASSINI SCLK
!              naif0012.tls           Leapseconds
!              04153_04182ca_ISS.bc   CASSINI image navigated
!                                     spacecraft CK
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'naif0012.tls',
!                                  'cas00071.tsc'
!                                  '04153_04182ca_ISS.bc' )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM CKMETA_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!        C     -- The code for CASSINI spacecraft reference frame is
!        C        -82000.
!        C
!        C     -- The reference frame we want is J2000.
!        C
!              CHARACTER*(*)         REF
!              PARAMETER           ( REF = 'J2000' )
!
!              INTEGER               CKID
!              PARAMETER           ( CKID = -82000  )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      AV     ( 3 )
!              DOUBLE PRECISION      CLKOUT
!              DOUBLE PRECISION      CMAT   ( 3, 3 )
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      ETOUT
!              DOUBLE PRECISION      SECTOL
!              DOUBLE PRECISION      TICKS
!              DOUBLE PRECISION      TICK2
!              DOUBLE PRECISION      TOL
!
!              INTEGER               IDCODE
!
!              LOGICAL               FOUND
!
!        C
!        C     Initial values.
!        C
!              DATA                  ET      / 141162208.034340D0 /
!              DATA                  SECTOL  / 0.5D0 /
!
!
!        C
!        C     First load the CK, LSK and SCLK files.
!        C
!              CALL FURNSH ( 'ckmeta_ex1.tm' )
!
!        C
!        C     Get the SCLK identifier of the spacecraft clock required
!        C     to convert from ET to TICKS.
!        C
!              CALL CKMETA ( CKID, 'SCLK', IDCODE )
!
!        C
!        C     Convert ET and ET+SECTOL to spacecraft clock ticks.
!        C
!              CALL SCE2C  ( IDCODE, ET,        TICKS  )
!              CALL SCE2C  ( IDCODE, ET+SECTOL, TICK2  )
!
!
!        C
!        C     Compute the tolerance in spacecraft clock ticks.
!        C
!              TOL = TICK2 - TICKS
!
!        C
!        C     Look the attitude up.
!        C
!              CALL CKGPAV ( CKID, TICKS, TOL,    REF,
!             .              CMAT, AV,    CLKOUT, FOUND )
!
!              WRITE(*,'(A,F20.6)')    'Input ET:            ', ET
!
!              IF ( FOUND ) THEN
!
!                 CALL SCT2E ( IDCODE, CLKOUT, ETOUT )
!                 WRITE(*,'(A,F20.6)') 'Attitude found at ET:', ETOUT
!
!              ELSE
!
!                 WRITE(*,'(A)') 'No attitude found at ET.'
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
!        Input ET:                141162208.034340
!        Attitude found at ET:    141162208.034586
!
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 07-JUN-2021 (JDR) (NJB)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing fragment.
!
!-    SPICELIB Version 1.2.0, 06-SEP-2013 (BVS)
!
!        BUG FIX: the POOL agents now watch both variables --
!        CK_<ID>_SCLK and CK_<ID>_SPK. Before they watched only
!        CK_<ID>_SCLK.
!
!        BUG FIX: if a previously available CK_<ID>_SCLK or CK_<ID>_SPK
!        variable that was used to populate a saved value disappears,
!        the routine now resets and returns the value based on the
!        default rule rather than keeping and returning the stale
!        POOL-based saved value.
!
!        BUG FIX: the routine now deletes watchers for the CK IDs that
!        were bumped from the local buffer.
!
!        Updated to keep track of agent-specific POOL counters and call
!        ZZCVPOOL to make use of them.
!
!-    SPICELIB Version 1.1.0, 05-MAR-2009 (NJB)
!
!        This routine now keeps track of whether its kernel pool
!        look-up failed. If so, a kernel pool lookup is attempted on
!        the next call to this routine. This change is an enhancement,
!        not a bug fix (unlike similar modifications in SCLK routines).
!
!        Header sections were put in correct order.
!
!-    SPICELIB Version 1.0.1, 09-MAR-1999 (NJB)
!
!        Comments referring to SCE2T have been updated to refer to
!        SCE2C. Occurrences of "id" replaced by "ID."
!
!-    SPICELIB Version 1.0.0, 04-OCT-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     Map C-kernel ID to SCLK and SPK ID
!
!-&


!
!     SPICELIB Functions
!
      integer(ip)           bschoi

      logical               failed
      logical               return


!
!     Local parameters
!
      integer(ip), parameter :: wdsize = 32

      integer(ip), parameter :: nck = 30

      integer(ip), parameter :: sclk = 1

      integer(ip), parameter :: spk = sclk   + 1

!
!     Local variables
!
      character(7)          base
      character(7)          mymeta
      character(wdsize)     lookup ( 2, nck )
      character(wdsize)     agent  (    nck )


      integer(ip)           cks    (    nck )
      integer(ip)           cksord (    nck )
      integer(ip)           currnt
      integer(ip)           last
      integer(ip)           n
      integer(ip)           sclks  (    nck )
      integer(ip)           spks   (    nck )
      integer(ip)           this
      integer(ip)           usrctr ( ctrsiz, nck )

      logical               first
      logical               found  (    2   )
      logical               nodata
      logical               update

!
!     Saved variables
!
      save

!
!     Initial values
!
      data                  base   / 'CKMETA.' /
      data                  currnt / 0         /
      data                  first  / .true.    /
      data                  last   / 0         /
      data                  nodata / .true.    /

!
!     Standard SPICE error handling.
!
      if ( return() ) return


      if ( first ) then

!
!        Initialize all agent-specific POOL counters to user value.
!
         do n = 1, nck
            call zzctruin( usrctr( 1, n ) )
         end do

!
!        Clear AGENTS array. We will use a non-blank AGENT value as the
!        flag to delete previously set watchers.
!
         call clearc ( nck, agent )

         first = .false.

      end if

!
!     Get an upper-case, left-justified copy of the metadata
!     type ('SCLK' or 'SPK').
!
      call ljucrs ( 1, meta, mymeta )

!
!     See if we already have this CK ID in hand.
!
      this = bschoi ( ckid, currnt, cks, cksord )

      if ( this > 0 ) then
!
!        We've got it.  Check to see if its value has been updated.
!        (Note that every CK ID  has its own agent and saved POOL
!        counter.)
!
         call zzcvpool ( agent( this ), usrctr( 1, this ), update )

         if ( update .or. nodata ) then

            call gipool( lookup(sclk,this), 1, 1, n, &
                         sclks(this),       found(sclk) )

            call gipool( lookup(spk, this), 1, 1, n, &
                         spks (this),       found(spk) )

            if ( failed() ) then

               nodata = .true.

               return

            end if

!
!           Note that failure to find data is not an error in this
!           routine; it's just SPICE errors that are a problem.
!
            nodata = .false.

         else

!
!           The POOL variables did not change since the last check and
!           we have already buffered IDs for this CK ID. Set found
!           flags to make use of saved values.
!
            found(sclk) = .true.
            found(spk)  = .true.

         end if


      else

!
!        We don't have this on our handy list. Find a place to put it.
!
         if ( currnt < nck ) then

            currnt = currnt + 1
            last   = currnt

         else

            last   = last + 1

            if ( last > nck ) then
               last = 1
            end if

         end if

         this = last

!
!        If we already have a watcher at this index, delete it. Note
!        we may have an update pending for this watcher, so we will
!        check it first to clear it.
!
         if ( agent( this ) /= ' ' ) then
            call cvpool ( agent( this ), update )
            call dwpool ( agent( this ) )
         end if

!
!        Recompute the order vector for the CKS; construct the
!        kernel pool variable names and the agent name.
!
         cks(this) = ckid

         call orderi (  cks,    currnt, cksord            )

         call intstr (  ckid,           lookup(sclk,this) )
         call prefix ( 'CK_',   0,      lookup(sclk,this) )

         agent (     this) =  base   // lookup(sclk,this)
         lookup(spk, this) =            lookup(sclk,this)

         call suffix ( '_SCLK',  0,     lookup(sclk,this) )
         call suffix ( '_SPK',   0,     lookup(spk, this) )

!
!        Set a watch for this item and fetch the current value
!        from the kernel pool (if there is a value there).
!
         call swpool ( agent( this ), 2, lookup(1,this) )

         call cvpool ( agent( this ), update )

         call gipool (lookup(sclk,this), 1, 1, n, &
                      sclks(this),       found(sclk) )

         call gipool (lookup(spk, this), 1, 1, n, &
                      spks (this),       found(spk ) )


         if ( failed() ) then

            nodata = .true.

            return

         end if

!
!        Note that failure to find data is not an error in this
!        routine; it's just SPICE errors that are a problem.
!
!        At this point, kernel data checks are done.
!
         nodata = .false.

      end if

!
!     If we didn't find either _SCLK or _SPK variable, we manufacture
!     an ID code based upon the "convention" used for all CKS so far.
!     However, the convention assumes that the CK ID will be less than
!     -1000 if it's not there is no sensible ID to return.  We return
!     zero in that case.
!
      if ( .not. found(sclk) ) then

         if ( cks(this) <= -1000 ) then

            sclks(this) = cks(this)/1000

         else

            sclks(this) = 0

         end if

      end if

      if ( .not. found(spk) ) then

         if ( cks(this) <= -1000 ) then

            spks(this) = cks(this)/1000

         else

            spks(this) = 0

         end if

      end if

!
!     Set output ID.
!
      if ( mymeta == 'SPK' ) then

         idcode = spks (this)

      else if ( mymeta == 'SCLK' ) then

         idcode = sclks(this)

      else

         idcode = 0

         call setmsg ( 'The CK meta data item "#" is not a ' &
         //            'recognized meta data item for the ' &
         //            'routine CKMETA. The recognized value ' &
         //            'are "SPK" and "SCLK". ' )

         call errch  ( '#', meta )
         call sigerr ( 'SPICE(UNKNOWNCKMETA)'  )
         return

      end if


      return
      end
