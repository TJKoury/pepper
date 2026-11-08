!$Procedure STELAB     ( Stellar Aberration )

      subroutine stelab ( pobj, vobs, appobj )

! $ Abstract
!
!     Correct the apparent position of an object for stellar
!     aberration.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      double precision   pobj   ( 3 )
      double precision   vobs   ( 3 )
      double precision   appobj ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     POBJ       I   Position of an object with respect to the
!                    observer.
!     VOBS       I   Velocity of the observer with respect to the
!                    Solar System barycenter.
!     APPOBJ     O   Apparent position of the object with respect to
!                    the observer, corrected for stellar aberration.
!
! $ Detailed_Input
!
!     POBJ     is the position (x, y, z, km) of an object with
!              respect to the observer, possibly corrected for
!              light time.
!
!     VOBS     is the velocity (dx/dt, dy/dt, dz/dt, km/sec)
!              of the observer with respect to the Solar System
!              barycenter.
!
! $ Detailed_Output
!
!     APPOBJ   is the apparent position of the object relative
!              to the observer, corrected for stellar aberration.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the velocity of the observer is greater than or equal
!         to the speed of light, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Let r be the vector from the observer to the object, and v be
!         -                                                    -
!     the velocity of the observer with respect to the Solar System
!     barycenter. Let w be the angle between them. The aberration
!     angle phi is given by
!
!          sin(phi) = v sin(w) / c
!
!     Let h be the vector given by the cross product
!         -
!
!           h = r X v
!           -   -   -
!
!     Rotate r by phi radians about h to obtain the apparent position
!            -                      -
!     of the object.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Compute the apparent position of the Moon relative to the
!        Earth, corrected for one light-time and stellar aberration,
!        given the geometric state of the Earth relative to the Solar
!        System Barycenter, and the difference between the stellar
!        aberration corrected and uncorrected position vectors, taking
!        several steps.
!
!        First, compute the light-time corrected state of the Moon body
!        as seen by the Earth, using its geometric state. Then apply
!        the correction for stellar aberration to the light-time
!        corrected state of the target body.
!
!        The code in this example could be replaced by a single call
!        to SPKPOS:
!
!            CALL SPKPOS ( 'MOON', ET, 'J2000', 'LT+S', 'EARTH',
!           .               POS,   LT                           )
!
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: stelab_ex1.tm
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
!              File name                     Contents
!              ---------                     --------
!              de418.bsp                     Planetary ephemeris
!              naif0009.tls                  Leapseconds
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de418.bsp',
!                                  'naif0009.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM STELAB_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(6)         REFFRM
!              CHARACTER*(12)        UTCSTR
!
!              DOUBLE PRECISION      APPDIF ( 3 )
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      PCORR  ( 3 )
!              DOUBLE PRECISION      POS    ( 3 )
!              DOUBLE PRECISION      SOBS   ( 6 )
!
!              INTEGER               IDOBS
!              INTEGER               IDTARG
!
!        C
!        C     Assign an observer, Earth, target, Moon, time of interest
!        C     and reference frame for returned vectors.
!        C
!              IDOBS  = 399
!              IDTARG = 301
!              UTCSTR = 'July 4 2004'
!              REFFRM = 'J2000'
!
!        C
!        C     Load the needed kernels.
!        C
!              CALL FURNSH ( 'stelab_ex1.tm' )
!
!        C
!        C     Convert the time string to ephemeris time, J2000.
!        C
!              CALL STR2ET ( UTCSTR, ET )
!
!        C
!        C     Get the state of the observer with respect to the solar
!        C     system barycenter.
!        C
!              CALL SPKSSB ( IDOBS, ET, REFFRM, SOBS )
!
!        C
!        C     Get the light-time corrected position POS of the target
!        C     body IDTARG as seen by the observer.
!        C
!              CALL SPKAPO ( IDTARG, ET, REFFRM, SOBS, 'LT', POS, LT )
!
!        C
!        C     Output the uncorrected vector.
!        C
!              WRITE(*,*) 'Uncorrected position vector'
!              WRITE(*,'(A,3F19.6)') '   ', POS(1), POS(2), POS(3)
!
!        C
!        C     Apply the correction for stellar aberration to the
!        C     light-time corrected position of the target body.
!        C
!              CALL STELAB ( POS, SOBS(4), PCORR )
!
!        C
!        C     Output the corrected position vector and the apparent
!        C     difference from the uncorrected vector.
!        C
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Corrected position vector'
!              WRITE(*,'(A,3F19.6)') '   ', PCORR(1), PCORR(2),
!             .                             PCORR(3)
!
!        C
!        C     Apparent difference.
!        C
!              CALL VSUB ( POS, PCORR, APPDIF )
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Apparent difference'
!              WRITE(*,'(A,3F19.6)') '   ', APPDIF(1), APPDIF(2),
!             .                            APPDIF(3)
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Uncorrected position vector
!                 201738.725087     -260893.141602     -147722.589056
!
!         Corrected position vector
!                 201765.929516     -260876.818077     -147714.262441
!
!         Apparent difference
!                    -27.204429         -16.323525          -8.326615
!
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  W. Owen, "The Treatment of Aberration in Optical Navigation",
!          JPL IOM #314.8-524, 8 February 1985.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 05-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added example's
!        meta-kernel and problem statement. Created complete code
!        example from existing code fragments.
!
!-    SPICELIB Version 1.1.1, 08-JAN-2008 (NJB)
!
!        The header example was updated to remove references
!        to SPKAPP.
!
!-    SPICELIB Version 1.1.0, 08-FEB-1999 (WLT)
!
!        The example was corrected so that SOBS(4) is passed
!        into STELAB instead of STARG(4).
!
!-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.1, 08-AUG-1990 (HAN)
!
!        $Examples section of the header was updated to replace
!        calls to the GEF ephemeris readers by calls to the
!        new SPK ephemeris reader.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN)
!
!-&


! $ Index_Entries
!
!     stellar aberration
!
!-&


! $ Revisions
!
!-    Beta Version 2.1.0, 9-MAR-1989 (HAN)
!
!        Declaration of the variable LIGHT was removed from the code.
!        The variable was declared but never used.
!
!-    Beta Version 2.0.0, 28-DEC-1988 (HAN)
!
!        Error handling was added to check the velocity of the
!        observer. If the velocity of the observer is greater
!        than or equal to the speed of light, the error
!        SPICE(VALUEOUTOFRANGE) is signaled.
!
!-&


!
!     SPICELIB functions
!
      double precision      clight
      double precision      vdot
      double precision      vnorm
      logical               return


!
!     Local variables
!

      double precision      onebyc
      double precision      u      ( 3 )
      double precision      vbyc   ( 3 )
      double precision      lensqr
      double precision      h      ( 3 )
      double precision      sinphi
      double precision      phi


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'STELAB' )
      end if


!
!     We are not going to compute the aberrated vector in exactly the
!     way described in the particulars section.  We can combine some
!     steps and we take some precautions to prevent floating point
!     overflows.
!
!
!     Get a unit vector that points in the direction of the object
!     ( u_obj ).
!
      call vhat ( pobj, u )

!
!     Get the velocity vector scaled with respect to the speed of light
!     ( v/c ).
!
      onebyc = 1.0d0 / clight()

      call vscl  ( onebyc, vobs, vbyc )

!
!     If the square of the length of the velocity vector is greater than
!     or equal to one, the speed of the observer is greater than or
!     equal to the speed of light. The observer speed is definitely out
!     of range. Signal an error and check out.
!
      lensqr = vdot ( vbyc, vbyc )

      if ( lensqr .ge. 1.0d0 ) then

         call setmsg ( 'Velocity components of observer were:  '      // &
                       'dx/dt = *, dy/dt = *, dz/dt = *.'              )
         call errdp  ( '*', vobs (1)            )
         call errdp  ( '*', vobs (2)            )
         call errdp  ( '*', vobs (3)            )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         call chkout ( 'STELAB'                 )
         return

      end if

!
!     Compute u_obj x (v/c)
!
      call vcrss ( u, vbyc, h )


!
!     If the magnitude of the vector H is zero, the observer is moving
!     along the line of sight to the object, and no correction is
!     required. Otherwise, rotate the position of the object by phi
!     radians about H to obtain the apparent position.
!
      sinphi  = vnorm ( h )

      if ( sinphi .ne. 0.d0 ) then

         phi = dasin ( sinphi )
         call  vrotv ( pobj, h, phi, appobj )

      else

         call moved ( pobj, 3, appobj )

      end if


      call chkout ( 'STELAB' )
      return
      end
