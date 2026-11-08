!$Procedure UNITIM ( Uniform time scale transformation )

      real(wp) function unitim ( epoch, insys, outsys )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Transform time from one uniform scale to another. The uniform
!     time scales are TAI, GPS, TT, TDT, TDB, ET, JED, JDTDB, JDTDT.
!
! $ Required_Reading
!
!     TIME
!
! $ Keywords
!
!     CONVERSION
!     TIME
!     UTILITY
!
! $ Declarations

      implicit none

      include               'zzctr.inc'

      real(wp)              epoch
      character(*)          insys
      character(*)          outsys

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     EPOCH      I   An epoch.
!     INSYS      I   The time scale associated with the input EPOCH.
!     OUTSYS     I   The time scale associated with the function value.
!
!     The function returns the d.p. in OUTSYS that is equivalent to the
!     EPOCH on the INSYS time scale.
!
! $ Detailed_Input
!
!     EPOCH    is an epoch relative to the INSYS time scale.
!
!     INSYS    is a time scale. Acceptable values are:
!
!                 'TAI'     International Atomic Time.
!                 'TDB'     Barycentric Dynamical Time.
!                 'TDT'     Terrestrial Dynamical Time.
!                 'TT'      Terrestrial Time, identical to TDT.
!                 'ET'      Ephemeris time (in the SPICE system, this is
!                           equivalent to TDB).
!                 'JDTDB'   Julian Date relative to TDB.
!                 'JDTDT'   Julian Date relative to TDT.
!                 'JED'     Julian Ephemeris date (in the SPICE system
!                           this is equivalent to JDTDB).
!                 'GPS'     Global Positioning System Time.
!
!              The routine is not sensitive to the case of the
!              characters in INSYS; 'tai' 'Tai' and 'TAI' are all
!              equivalent from the point of view of this routine.
!
!     OUTSYS   is the time scale to which EPOCH should be converted.
!              Acceptable values are the same as for INSYS. The
!              routine is not sensitive to the case of OUTSYS.
!
! $ Detailed_Output
!
!     The function returns the time in the system specified by OUTSYS
!     that is equivalent to the EPOCH in the INSYS time scale.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The kernel pool must contain the variables:
!
!            'DELTET/DELTA_T_A'
!            'DELTET/K'
!            'DELTET/EB'
!            'DELTET/M'
!
!         If these are not present, the error SPICE(MISSINGTIMEINFO) is
!         signaled. (These variables are typically inserted into the
!         kernel pool by loading a leapseconds kernel with the SPICE
!         routine FURNSH.)
!
!     2)  If the names of either the input or output time types are
!         unrecognized, the error SPICE(BADTIMETYPE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     We use the term uniform time scale to refer to those
!     representations of time that are numeric (each epoch is
!     represented by a number) and additive. A numeric time system is
!     additive if given the representations, E1 and E2, of any pair of
!     successive epochs, the time elapsed between the epochs is given by
!     E2 - E1.
!
!     Given an epoch in one of the uniform time scales specified by
!     INSYS, the function returns the equivalent representation in the
!     scale specified by OUTSYS. A list of the recognized uniform time
!     scales is given in the detailed input for INSYS.
!
! $ Examples
!
!     To convert an epoch with respect to the International Atomic
!     Time (TAI) scale to ET (Barycentric Dynamical Time), make the
!     following assignment.
!
!           ET = UNITIM ( TAI, 'TAI', 'ET' )
!
! $ Restrictions
!
!     1)  The appropriate variable must be loaded into the SPICE kernel
!         pool (normally by loading a leapseconds kernel with FURNSH)
!         prior to calling this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.5.0, 05-SEP-2020 (EDW) (JDR)
!
!        Added time system name 'TT' (Terrestrial Time) as alternate
!        assignment of 'TDT' (Terrestrial Dynamical Time).
!
!        Included GPS time system mapping.
!
!        Edited the header to comply with NAIF standard.
!
!        Removed references to FURNSH, CLPOOL, KCLEAR, UNLOAD, and
!        Required Reading documents and tutorials from the "variables
!        not present" long error message.
!
!-    SPICELIB Version 1.4.0, 09-SEP-2013 (BVS)
!
!        Updated to keep track of the POOL counter and call ZZCVPOOL.
!
!-    SPICELIB Version 1.3.0, 05-MAR-2009 (NJB)
!
!        This routine now keeps track of whether its kernel pool
!        look-up failed. If so, a kernel pool lookup is attempted on
!        the next call to this routine. This change is an enhancement,
!        not a bug fix (unlike similar modifications in SCLK routines).
!
!-    SPICELIB Version 1.2.1, 15-NOV-2006 (EDW) (NJB)
!
!        Replaced references to LDPOOL with references to FURNSH.
!        Replaced references to RTPOOL with references to GDPOOL.
!        Enhanced long error message associated with missing kernel
!        variables.
!
!-    SPICELIB Version 1.2.0, 17-FEB-1999 (WLT)
!
!        Added a second call to SWPOOL in the event some required
!        kernel pool variable is not supplied.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        If the value of the function RETURN is .TRUE. upon execution of
!        this module, this function is assigned a default value of
!        either 0, 0.0D0, .FALSE., or blank depending on the type of
!        the function.
!
!-    SPICELIB Version 1.0.0, 28-MAR-1992 (WLT)
!
!-&


! $ Index_Entries
!
!     Transform between two uniform numeric time systems
!     Transform between two additive numeric time systems
!     Convert one uniform numeric time system to another
!     Convert one additive numeric time system to another
!
!-&


!
!     SPICELIB functions
!
      real(wp)              j2000
      real(wp)              spd

      logical               elemc
      logical               failed
      logical               return
      logical               setc
      logical               somfls

!
!     Local parameters
!

!
!     LBCELL is the bottom slot of a cell.
!
      integer(ip)           lbcell
      parameter           ( lbcell = -5 )
!
!     NEEDED is the number of kernel pool variables needed by this
!     routine.
!
      integer(ip)           needed
      parameter           ( needed = 4 )

!
!     LNGVAR is the length of the longest kernel pool variable name
!     that is used by this routine.
!
      integer(ip)           lngvar
      parameter           ( lngvar = 16 )

!
!     MISLEN is the length required by the MISSED array of strings
!     used for error messages.
!
      integer(ip)           mislen
      parameter           ( mislen = 20 )
!
!     TYPLEN is the maximum length allowed for names of uniform
!     time types.
!
      integer(ip)           typlen
      parameter           ( typlen = 8 )

!
!     NTDT is the number of time types based on terrestrial dynamical
!     time (TDT).
!
      integer(ip)           ntdt
      parameter           ( ntdt   = 5 )

!
!     NTDB is the number of time types base on barycentric dynamical
!     time (TDB).
!
      integer(ip)           ntdb
      parameter           ( ntdb   = 4 )


!
!     NRECOG is the total number of recognized types.
!
      integer(ip)           nrecog
      parameter           ( nrecog = ntdb + ntdt )

!
!     Constant shift between GPS time system and TAI time system.
!
      real(wp)              gpstai
      parameter           ( gpstai = 19.0_wp )


!
!     Local variables
!
      character(1)          bslash
      character(lngvar)     vars   ( needed )
      character(mislen)     missed ( needed )
      character(typlen)     myin
      character(typlen)     myout
      character(typlen)     recog  ( lbcell : nrecog    )
      character(typlen)     types  ( lbcell : 2         )
      character(typlen)     typtdb ( lbcell : ntdb      )
      character(typlen)     typtdt ( lbcell : ntdt      )

      real(wp)              dta
      real(wp)              eb
      real(wp)              jd2000
      real(wp)              k
      real(wp)              m     ( 0:1 )
      real(wp)              mytime
      real(wp)              secspd
      real(wp)              tdb
      real(wp)              tdt

      integer(ip)           i
      integer(ip)           n
      integer(ip)           usrctr ( ctrsiz )

      logical               first
      logical               found    ( needed )
      logical               intdb
      logical               intdt
      logical               nodata
      logical               outtdb
      logical               outtdt
      logical               update


!
!     Saved variables
!
      save                  bslash
      save                  dta
      save                  eb
      save                  first
      save                  jd2000
      save                  k
      save                  m
      save                  missed
      save                  nodata
      save                  recog
      save                  secspd
      save                  typtdb
      save                  typtdt
      save                  vars
      save                  usrctr

!
!     Initial values
!
      data                  first    / .true. /

      data                  missed   / 'DELTET/DELTA_T_A, #', &
                                       'DELTET/K, #', &
                                       'DELTET/EB, #', &
                                       'DELTET/M, #'           /

      data                  nodata   / .true. /

      data                  vars     / 'DELTET/DELTA_T_A', &
                                       'DELTET/K', &
                                       'DELTET/EB', &
                                       'DELTET/M'              /

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         unitim = 0.0_wp
         return
      end if

      call chkin ( 'UNITIM' )


      if ( first ) then

         first  = .false.
!
!        Initialize the backslash character.  We use this for error
!        message construction.
!
         bslash = char(92)

!
!        Set up the parameters that we are going to need often.
!
         secspd  = spd()
         jd2000  = j2000()

!
!        Initialize the sets that we will use.
!
         typtdt(1) = 'JDTDT'
         typtdt(2) = 'TAI'
         typtdt(3) = 'TDT'
         typtdt(4) = 'GPS'
         typtdt(5) = 'TT'

         typtdb(1) = 'ET'
         typtdb(2) = 'JDTDB'
         typtdb(3) = 'JED'
         typtdb(4) = 'TDB'

         call validc ( ntdt,    ntdt,   typtdt )
         call validc ( ntdb,    ntdb,   typtdb )

         call ssizec ( ntdt  +  ntdb,   recog  )
         call unionc ( typtdt,  typtdb, recog  )

!
!        Initialize the local POOL counter to user value.
!
         call zzctruin( usrctr )

!
!        Set up the kernel pool watchers
!
         call swpool ( 'UNITIM', needed, vars )

      end if

!
!     Check to see if any of the kernel items required by this
!     routine have been updated since the last call to this
!     entry point.
!
      call zzcvpool ( 'UNITIM', usrctr, update )

      if ( update .or. nodata ) then
!
!        Fetch all of the time parameters from the kernel pool.
!
         call gdpool ( 'DELTET/DELTA_T_A', 1, 1, n, dta,  found(1) )
         call gdpool ( 'DELTET/K',         1, 1, n, k,    found(2) )
         call gdpool ( 'DELTET/EB',        1, 1, n, eb,   found(3) )
         call gdpool ( 'DELTET/M',         1, 2, n, m,    found(4) )

         if ( failed() ) then

            nodata = .true.
            unitim = 0.0_wp

            call chkout ( 'UNITIM' )
            return

         end if

!
!        If anything wasn't found, it's an error dude.
!
         if ( somfls ( found, needed ) ) then

            nodata = .true.

!
!           If we didn't get all of the things we needed for time
!           conversion, we need to reset the watch.  Otherwise
!           subsequent calls to this routine will never have the
!           needed data.
!
            call swpool ( 'UNITIM', needed, vars )

            call setmsg ( 'The following variables, needed to convert'// &
                          ' between the input uniform time scales, '  // &
                          'were not found in the kernel pool: # '     // &
                          'Your program may have failed to load a '   // &
                          'leapseconds kernel. Other possible '       // &
                          'causes of this problem include loading an '// &
                          'invalid leapseconds kernel---one that '    // &
                          'lacks an initial '// bslash //'begindata ' // &
                          'marker or final newline character, or is ' // &
                          'otherwise corrupted---or deleting previous'// &
                          'ly loaded kernel pool variables via calls '// &
                          'to routines that clear the kernel pool. '  )

            do i = 1, needed
               if ( .not. found(i) ) then
                  call errch ( '#', missed(i) )
               end if
            end do

            call errch  ( ', #', '.' )
            call sigerr ( 'SPICE(MISSINGTIMEINFO)' )

            call chkout ( 'UNITIM' )
            unitim = epoch

            return

         end if

!
!        At this point the kernel data checks are done.
!
         nodata = .false.

      end if

!
!     Normalize the IN and OUT scale variables
!
      call ucase  ( insys,  myin  )
      call ucase  ( outsys, myout )

      call ssizec ( 2,      types )
      call insrtc ( myin,   types )
      call insrtc ( myout,  types )

!
!     We will work with a local copy of EPOCH.
!
      mytime = epoch

!
!     First make sure both types are recognized.
!
      if      ( .not. setc ( types, '<', recog ) ) then

         call setmsg ( 'The time types recognized by UNITIM are: '   // &
                   'TAI, GPS, TT, TDT, JDTDT, TDB, ET, JED, JDTDB. ' // &
                   'At least one of the inputs (#, #) was not in '   // &
                   'the list of recognized types. '               )

         call errch  ( '#', myin            )
         call errch  ( '#', myout           )
         call sigerr ( 'SPICE(BADTIMETYPE)' )
         call chkout ( 'UNITIM'             )

         unitim = epoch
         return

      end if

!
!     If the input and output types are the same, just copy the input
!     epoch to the output and call it quits.
!
      if ( myin == myout ) then

         unitim = mytime

         call chkout ( 'UNITIM' )
         return
      end if


!
!     Determine the base types of the input and output types.
!
      intdt  = elemc ( myin,  typtdt )
      outtdt = elemc ( myout, typtdt )
      intdb  = .not. intdt
      outtdb = .not. outtdt

!
!     The two types, TDT and TDB, will be used as the fundamental
!     base used in conversions.
!
!        TAI, GPS, and JDTDT will be converted to TDT
!        JED and JDTDB will be converted to TDB.
!        ET means TDB; TT means TDT.
!
!
      if      ( myin == 'TAI'   ) then

         mytime = mytime + dta

      else if ( myin == 'GPS' ) then

         mytime = mytime + (dta + gpstai)

      else if ( myin == 'JDTDT' ) then

         mytime = ( mytime - jd2000 ) * secspd

      else if ( myin == 'JED'   ) then

         mytime = ( mytime - jd2000 ) * secspd

      else if ( myin == 'JDTDB' ) then

         mytime = ( mytime - jd2000 ) * secspd

      end if

!
!     At this point, MYTIME has been converted from its input
!     to one of the base types.
!
!     Next change type from TDB to TDT or vice versa, if
!     required.  (The time is already in TDT or TDB).
!
      if      ( intdt .and. outtdb ) then

         tdt    = mytime
         tdb    = tdt + k*sin(           m(0) + m(1)*tdt &
                               + eb*sin( m(0) + m(1)*tdt ) )

         mytime = tdb

      else if ( intdb .and. outtdt ) then

!
!        What we have to do here is invert the formula used to get
!        TDB from TDT that was used above.
!
!        Of course solving the equation
!
!           TDB = TDT + K*SIN { M0 + M1*TDT + EB*SIN( MO + M1*TDT ) }
!
!        analytically for TDT if given TDB is no piece of cake.
!        However, we can get as close as we want to TDT if
!        we notice a few tricks.  First, let's let f(t) denote the
!        function
!
!           f(t) = SIN( M0 + M1*t + EB*SIN( M0 + M1*t ) )
!
!        With this simpler notation we can rewrite our problem
!        as that of solving the equation
!
!           y = t + K*f(t)
!
!        for t given y.  Whichever t satisfies this equation will be
!        unique. The uniqueness of the solution is ensured because the
!        expression on the right-hand side of the equation is
!        monotone increasing in t.
!
!        Let's suppose that t is the solution, then the following
!        is true.
!
!           t = y - K*f(t)
!
!        but we can also replace the t on the right hand side of the
!        equation by y - K*f(t).  Thus
!
!           t = y - K*f( y - K*f(t))
!
!             = y - K*f( y - K*f( y - K*f(t)))
!
!             = y - K*f( y - K*f( y - K*f( y - K*f(t))))
!
!             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(t)))))
!             .
!             .
!             .
!             = y - K*f( y - K*f( y - K*f( y - K*f( y - K*f(y - ... )))
!
!        and so on, for as long as we have patience to perform the
!        substitutions.
!
!        The point of doing this recursive substitution is that we
!        hope to move t to an insignificant part of the computation.
!        This would seem to have a reasonable chance of success since
!        K is a small number and f is bounded by 1.
!
!        Following this idea, we will attempt to solve for t using
!        the recursive method outlined below.
!
!        We will make our first guess at t, call it t_0.
!
!         t_0 = y
!
!        Our next guess, t_1, is given by:
!
!         t_1 = y - K*f(t_0)
!
!        And so on:
!
!         t_2 = y - K*f(t_1)        [ = y - K*f(y - K*f(y))            ]
!         t_3 = y - K*f(t_2)        [ = y - K*f(y - K*f(y - K*f(y)))   ]
!             .
!             .
!             .
!         t_n = y - K*f(t_(n-1))    [ = y - K*f(y - K*f(y - K*f(y...)))]
!
!        The questions to ask at this point are:
!
!           1) Do the t_i's converge?
!           2) If they converge, do they converge to t?
!           3) If they converge to t, how fast do they get there?
!
!        1) The sequence of approximations converges.
!
!           | t_n - t_(n-1) | =    [ y - K*f( t_(n-1) ) ]
!                               -  [ y - K*f( t_(n-2) ) ]
!
!                             =  K*[ f( t_(n-2) ) - f( t_(n-1) ) ]
!
!           The function f has an important property. The absolute
!           value of its derivative is always less than M1*(1+EB).
!           This means that for any pair of real numbers s,t
!
!              | f(t) - f(s) |  < M1*(1+EB)*| t - s |.
!
!           From this observation, we can see that
!
!             | t_n - t_(n-1) | < K*M1*(1+EB)*| t_(n-1) - t_(n-2) |
!
!           With this fact available, we could (with a bit more work)
!           conclude that the sequence of t_i's converges and that
!           it converges at a rate that is at least as fast as the
!           sequence L, L**2, L**3, ....
!
!           Where L = K*M1*(1+EB) << 1.
!
!         2) If we let t be the limit of the t_i's then it follows
!            that
!
!               t = y - K*f(t).
!
!            or that
!
!               y = t + K*f(t).
!
!         3) As we already pointed out, the sequence of t_i's
!            converges at least as fast as the geometric series
!            L, L**2, ...
!
!
!        Since K*M1*(1+EB) is quite small (on the order of 10**-9)
!        3 iterations should get us as close as we can get to the
!        solution for TDT
!

         tdb = mytime
         tdt = tdb

         do i = 1, 3

            tdt = tdb - k*sin(           m(0) + m(1)*tdt &
                               + eb*sin( m(0) + m(1)*tdt ))
         end do

         mytime = tdt

      end if

!
!     Now MYTIME is in the base type of the requested output.
!     If further conversion is required, we do it here.
!
      if      ( myout == 'TAI'   ) then

         mytime = mytime - dta

       else if ( myout == 'GPS' ) then

         mytime = mytime - (dta + gpstai)

      else if ( myout == 'JDTDT' ) then

         mytime = mytime/secspd + jd2000

      else if ( myout == 'JED'   ) then

         mytime = mytime/secspd + jd2000

      else if ( myout == 'JDTDB' ) then

         mytime = mytime/secspd + jd2000

      end if

      unitim = mytime

      call chkout ( 'UNITIM' )
      return
      end
