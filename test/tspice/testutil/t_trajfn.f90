!$Procedure T_TRAJFN ( Test utility, evaluate trajectory functions )

      DOUBLE PRECISION FUNCTION T_TRAJFN ( TARG, ET, REF, ABCORR, OBS )
      IMPLICIT NONE

! $ Abstract
!
!     Umbrella routine for function entry points that return state
!     components, light time and light time rate, and stellar
!     aberration correction components and their rates.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations

      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      INTEGER               OBS

      INCLUDE 'zzabcorr.inc'

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TARG       I   Target body ID code.
!     ET         I   Epoch.
!     REF        I   Reference frame name.
!     ABCORR     I   Aberration correction.
!     OBS        I   Observer ID code.
!
! $ Detailed_Input
!
!     TARG           is the NAIF ID code of the target body.
!
!     ET             is the time at which to evaluate a
!                    state component, light time or light time
!                    rate, or stellar aberration correction
!                    component. ET is expressed as seconds
!                    past J2000 TDB.
!
!     REF            is the reference frame in which to
!                    compute a state, aberration correction,
!                    or aberration correction rate.
!
!     ABCORR         is an aberration correction specification
!                    string. Any value acceptable to SPKEZ
!                    may be used.
!
!     OBS            is the NAIF ID of the observer body.
!
! $ Detailed_Output
!
!     The outputs of this routine are the values returned by this
!     routine's function entry points. See those entry points
!     for details.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by SPKEZ may be incurred
!        by this routine.
!
!     2) Any exceptions that may be incurred by SPKLTC may be incurred
!        by this routine.
!
!     3) Any exceptions that may be incurred by the test utility
!        T_GETSA may be incurred by this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine contains 15 function entry points which allow a
!     caller to fetch state components, stellar aberration correction
!     components, and light time and light time rate values via
!     function calls. These entry points are suitable for use with the
!     SUPPORT library Chebyshev fitting routine CHBFIT.
!
!     To use this package, an application should call the entry point
!
!        T_STCINI
!
!     to set the saved variables defining a state computation:
!
!        observer
!        target
!        aberration correction
!        reference frame.
!
!     After this, any of the state or aberration correction computation
!     functions can be called with an ET (seconds past J2000 TDB) input
!     value.
!
!     The entry points and their descriptions are listed below.
!
!
!     T_GETDLT {Get light time derivative}
!     T_GETDSX {Get stellar aberration correction velocity X-component}
!     T_GETDSY {Get stellar aberration correction velocity Y-component}
!     T_GETDSZ {Get stellar aberration correction velocity Z-component}
!     T_GETDX  {Get velocity X-component}
!     T_GETDY  {Get velocity Y-component}
!     T_GETDZ  {Get velocity Z-component}
!     T_GETLT  {Get light time}
!     T_GETSX  {Get stellar aberration correction X-component}
!     T_GETSY  {Get stellar aberration correction Y-component}
!     T_GETSZ  {Get stellar aberration correction Z-component}
!     T_GETX   {Get position X-component}
!     T_GETY   {Get position Y-component}
!     T_GETZ   {Get position Z-component}
!     T_STCINI {Initialize state and aberration correction computations}
!
!
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
!
! $ Restrictions
!
!     Due to the large number and trivial nature of the entry points,
!     these functions have very abbreviated header comments.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 26-DEC-2007 (NJB)
!
!-&


!
!     Entry points
!
      DOUBLE PRECISION      T_GETDLT
      DOUBLE PRECISION      T_GETDSX
      DOUBLE PRECISION      T_GETDSY
      DOUBLE PRECISION      T_GETDSZ
      DOUBLE PRECISION      T_GETDX
      DOUBLE PRECISION      T_GETDY
      DOUBLE PRECISION      T_GETDZ
      DOUBLE PRECISION      T_GETLT
      DOUBLE PRECISION      T_GETSX
      DOUBLE PRECISION      T_GETSY
      DOUBLE PRECISION      T_GETSZ
      DOUBLE PRECISION      T_GETX
      DOUBLE PRECISION      T_GETY
      DOUBLE PRECISION      T_GETZ
      DOUBLE PRECISION      T_STCINI


!
!     Local parameters
!
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local variables
!
      CHARACTER*(CORLEN)    SVCORR
      CHARACTER*(FRNMLN)    SVREF

      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DPCORR ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      PCORR  ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STOBS  ( 6 )

      INTEGER               SVOBS
      INTEGER               SVTARG


!
!     Saved variables
!
      SAVE


      T_TRAJFN = 0.D0

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      RETURN




!$Procedure T_STCINI ( Test utility, state computation initialization )

      ENTRY T_STCINI ( TARG, REF, ABCORR, OBS )

! $ Abstract
!
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     INTEGER               TARG
!     CHARACTER*(*)         REF
!     CHARACTER*(*)         ABCORR
!     INTEGER               OBS
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TARG       I   Target body ID code.
!     REF        I   Reference frame name.
!     ABCORR     I   Aberration correction.
!     OBS        I   Observer ID code.
!
! $ Detailed_Input
!
!     TARG           is the NAIF ID code of the target body.
!
!     REF            is the reference frame in which to
!                    compute a state, aberration correction,
!                    or aberration correction rate.
!
!     ABCORR         is an aberration correction specification
!                    string. Any value acceptable to SPKEZ
!                    may be used.
!
!     OBS            is the NAIF ID of the observer body.
!
! $ Detailed_Output
!
!     This routine operates by side effects. It returns an
!     unspecfied double precision constant, since Fortran
!     requires all functions and function entry points to
!     return values.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by ZZPRSCOR may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     This function initializes a state, aberration correction, or
!     aberration correction rate computation by saving parameters,
!     other than time, required to define a state. This function should
!     be called prior to calling the other entry points.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 30-DEC-2007 (NJB)
!
!-&


!
!     Set the double precision return value, since one is
!     required.
!
      T_STCINI = 0.D0

!
!     Save the input arguments for later use.
!
      SVTARG = TARG
      SVREF  = REF
      SVCORR = ABCORR
      SVOBS  = OBS


      RETURN




!$Procedure T_GETX ( Test utility, get position X component )

      ENTRY T_GETX ( ET )

! $ Abstract
!
!     Return X component of position vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    whose X position component is requested. ET is
!                    expressed as seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This routine returns the X component of the position vector
!     defined by the input ET value and the saved observer, target,
!     reference frame, and aberration correction values initialized
!     by the last call to T_STCINI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by SPKEZ may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the state. Return the X component of position.
!
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETX = STATE(1)

      RETURN





!$Procedure T_GETY ( Test utility, get position Y component )

      ENTRY T_GETY ( ET )

! $ Abstract
!
!     Return Y component of position vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    whose Y position component is requested. ET is
!                    expressed as seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This routine returns the Y component of the position vector
!     defined by the input ET value and the saved observer, target,
!     reference frame, and aberration correction values initialized
!     by the last call to T_STCINI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by SPKEZ may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the state. Return the Y component of position.
!
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETY = STATE(2)

      RETURN



!$Procedure T_GETZ ( Test utility, get position Z component )

      ENTRY T_GETZ ( ET )

! $ Abstract
!
!     Return Z component of position vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    whose Z position component is requested. ET is
!                    expressed as seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This routine returns the Z component of the position vector
!     defined by the input ET value and the saved observer, target,
!     reference frame, and aberration correction values initialized
!     by the last call to T_STCINI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by SPKEZ may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the state. Return the Z component of position.
!
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETZ = STATE(3)

      RETURN




!$Procedure T_GETDX ( Test utility, get velocity X component )

      ENTRY T_GETDX ( ET )

! $ Abstract
!
!     Return X component of velocity vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    whose X velocity component is requested. ET is
!                    expressed as seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This routine returns the X component of the velocity vector
!     defined by the input ET value and the saved observer, target,
!     reference frame, and aberration correction values initialized
!     by the last call to T_STCINI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by SPKEZ may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the state. Return the X component of velocity.
!
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETDX = STATE(4)

      RETURN



!$Procedure T_GETDY ( Test utility, get velocity Y component )

      ENTRY T_GETDY ( ET )

! $ Abstract
!
!     Return Y component of velocity vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    whose Y velocity component is requested. ET is
!                    expressed as seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This routine returns the Y component of the velocity vector
!     defined by the input ET value and the saved observer, target,
!     reference frame, and aberration correction values initialized
!     by the last call to T_STCINI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by SPKEZ may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the state. Return the Y component of velocity.
!
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETDY = STATE(5)

      RETURN



!$Procedure T_GETDZ ( Test utility, get velocity Z component )

      ENTRY T_GETDZ ( ET )

! $ Abstract
!
!     Return Z component of velocity vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    whose Z velocity component is requested. ET is
!                    expressed as seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This routine returns the Z component of the velocity vector
!     defined by the input ET value and the saved observer, target,
!     reference frame, and aberration correction values initialized
!     by the last call to T_STCINI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by SPKEZ may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the state. Return the Z component of velocity.
!
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETDZ = STATE(6)

      RETURN




!$Procedure T_GETSX ( Test utility, get S.A. position X component )

      ENTRY T_GETSX ( ET )

! $ Abstract
!
!     Return X component of the stellar aberration correction
!     position vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    with which the specified stellar aberration
!                    correction is associated. ET is expressed as
!                    seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This function returns the X component of the stellar aberration
!     correction position vector of the state defined by the input time
!     ET and the parameters saved from the last call to the
!     initialization entry point T_STCINI.
!
!     The stellar aberration correction position vector is the
!     difference between the geometric or light time corrected position
!     computed by SPKTLC for the given inputs, and this position after
!     the stellar aberration correction has been applied.
!
!     Note that if the saved aberration correction is NONE, than the
!     stellar aberration correction will be applied to the geometric
!     state vector computed by SPKEZ.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by T_GETSA may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     See the test utility T_GETSA for details of how the stellar
!     aberration correction position vector is computed.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the stellar aberration correction offset PCORR and its
!     derivative with respect to time DPCORR. Return the X component of
!     the stellar aberration correction position offset.
!
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETSX = PCORR(1)

      RETURN



!$Procedure T_GETSY ( Test utility, get S.A. position Y component )

      ENTRY T_GETSY ( ET )

! $ Abstract
!
!     Return Y component of the stellar aberration correction
!     position vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    with which the specified stellar aberration
!                    correction is associated. ET is expressed as
!                    seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This function returns the Y component of the stellar aberration
!     correction position vector of the state defined by the input time
!     ET and the parameters saved from the last call to the
!     initialization entry point T_STCINI.
!
!     The stellar aberration correction position vector is the
!     difference between the geometric or light time corrected position
!     computed by SPKTLC for the given inputs, and this position after
!     the stellar aberration correction has been applied.
!
!     Note that if the saved aberration correction is NONE, than the
!     stellar aberration correction will be applied to the geometric
!     state vector computed by SPKEZ.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by T_GETSA may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     See the test utility T_GETSA for details of how the stellar
!     aberration correction position vector is computed.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the stellar aberration correction offset PCORR and its
!     derivative with respect to time DPCORR. Return the Y component of
!     the stellar aberration correction position offset.
!
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETSY = PCORR(2)
      RETURN



!$Procedure T_GETSZ ( Test utility, get S.A. position Z component )

      ENTRY T_GETSZ ( ET )

! $ Abstract
!
!     Return Z component of the stellar aberration correction
!     position vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    with which the specified stellar aberration
!                    correction is associated. ET is expressed as
!                    seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This function returns the Z component of the stellar aberration
!     correction position vector of the state defined by the input time
!     ET and the parameters saved from the last call to the
!     initialization entry point T_STCINI.
!
!     The stellar aberration correction position vector is the
!     difference between the geometric or light time corrected position
!     computed by SPKTLC for the given inputs, and this position after
!     the stellar aberration correction has been applied.
!
!     Note that if the saved aberration correction is NONE, than the
!     stellar aberration correction will be applied to the geometric
!     state vector computed by SPKEZ.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by T_GETSA may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     See the test utility T_GETSA for details of how the stellar
!     aberration correction position vector is computed.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the stellar aberration correction offset PCORR and its
!     derivative with respect to time DPCORR. Return the Z component of
!     the stellar aberration correction position offset.
!
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETSZ = PCORR(3)

      RETURN




!$Procedure T_GETDSX ( Test utility, get S.A. velocity X component )

      ENTRY T_GETDSX ( ET )

! $ Abstract
!
!     Return X component of the stellar aberration correction
!     velocity vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    with which the specified stellar aberration
!                    correction is associated. ET is expressed as
!                    seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This function returns the X component of the stellar aberration
!     correction velocity vector of the state defined by the input time
!     ET and the parameters saved from the last call to the
!     initialization entry point T_STCINI.
!
!     The stellar aberration correction position vector is the
!     difference between the geometric or light time corrected position
!     computed by SPKTLC for the given inputs, and this position after
!     the stellar aberration correction has been applied. The stellar
!     aberration correction velocity vector is simply the derivative
!     with respect to time (TDB) of this position vector.
!
!     Note that if the saved aberration correction is NONE, than the
!     stellar aberration correction will be applied to the geometric
!     state vector computed by SPKEZ.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by T_GETSA may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     See the test utility T_GETSA for details of how the stellar
!     aberration correction velocity vector is computed.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the stellar aberration correction offset PCORR and its
!     derivative with respect to time DPCORR. Return the X component of
!     the stellar aberration correction velocity offset.
!
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETDSX = DPCORR(1)

      RETURN




!$Procedure T_GETDSY ( Test utility, get S.A. velocity Y component )

      ENTRY T_GETDSY ( ET )

! $ Abstract
!
!     Return Y component of the stellar aberration correction
!     velocity vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    with which the specified stellar aberration
!                    correction is associated. ET is expressed as
!                    seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This function returns the Y component of the stellar aberration
!     correction velocity vector of the state defined by the input time
!     ET and the parameters saved from the last call to the
!     initialization entry point T_STCINI.
!
!     The stellar aberration correction position vector is the
!     difference between the geometric or light time corrected position
!     computed by SPKTLC for the given inputs, and this position after
!     the stellar aberration correction has been applied. The stellar
!     aberration correction velocity vector is simply the derivative
!     with respect to time (TDB) of this position vector.
!
!     Note that if the saved aberration correction is NONE, than the
!     stellar aberration correction will be applied to the geometric
!     state vector computed by SPKEZ.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by T_GETSA may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     See the test utility T_GETSA for details of how the stellar
!     aberration correction velocity vector is computed.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the stellar aberration correction offset PCORR and its
!     derivative with respect to time DPCORR. Return the Y component of
!     the stellar aberration correction velocity offset.
!
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETDSY = DPCORR(2)

      RETURN




!$Procedure T_GETDSZ ( Test utility, get S.A. velocity Z component )

      ENTRY T_GETDSZ ( ET )

! $ Abstract
!
!     Return Z component of the stellar aberration correction
!     velocity vector at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    with which the specified stellar aberration
!                    correction is associated. ET is expressed as
!                    seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This function returns the Z component of the stellar aberration
!     correction velocity vector of the state defined by the input time
!     ET and the parameters saved from the last call to the
!     initialization entry point T_STCINI.
!
!     The stellar aberration correction position vector is the
!     difference between the geometric or light time corrected position
!     computed by SPKTLC for the given inputs, and this position after
!     the stellar aberration correction has been applied. The stellar
!     aberration correction velocity vector is simply the derivative
!     with respect to time (TDB) of this position vector.
!
!     Note that if the saved aberration correction is NONE, than the
!     stellar aberration correction will be applied to the geometric
!     state vector computed by SPKEZ.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by T_GETSA may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     See the test utility T_GETSA for details of how the stellar
!     aberration correction velocity vector is computed.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the stellar aberration correction offset PCORR and its
!     derivative with respect to time DPCORR. Return the Z component of
!     the stellar aberration correction velocity offset.
!
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETDSZ = DPCORR(3)

      RETURN




!$Procedure T_GETLT ( Test utility, get light time )

      ENTRY T_GETLT ( ET )

! $ Abstract
!
!     Return observer-target light time at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    with which the requested light time is associated.
!                    ET is expressed as seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     This routine returns the one-way light time defined by the input
!     ET value and the saved observer, target, reference frame, and
!     aberration correction values initialized by the last call to
!     T_STCINI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by SPKLTC may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the light time corrected state, along with
!     the light time and the light time rate. Return the light
!     time.
!
      CALL SPKSSB ( SVOBS,  ET, SVREF,         STOBS  )
      CALL SPKLTC ( SVTARG, ET, SVREF, SVCORR, STOBS, &
                    STATE,  LT, DLT                   )
      T_GETLT =  LT

      RETURN





!$Procedure T_GETDLT ( Test utility, get light time rate )

      ENTRY T_GETDLT ( ET )

! $ Abstract
!
!     Return observer-target light time rate at ET.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch.
!
! $ Detailed_Input
!
!     ET             is the time at which to evaluate the state vector
!                    with which the requested light time rate is
!                    associated. ET is expressed as seconds past J2000
!                    TDB.
!
! $ Detailed_Output
!
!     This function returns the rate---that is, the derivative with
!     respect to time (TDB)---of one way light time associated with the
!     state defined by the input time ET and the parameters saved from
!     the last call to the initialization entry point T_STCINI.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions that may be incurred by SPKLTC may be incurred
!        by this routine.
!
! $ Files
!
!     See the header of SPKEZ.
!
! $ Particulars
!
!     The terms "rate" and "derivative with respect to time" are
!     synonyms.
!
! $ Examples
!
!     See the test family F_ABCORR for usage examples.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
!
!-&

!
!     Look up the light time corrected state, along with
!     the light time and the light time rate. Return the light
!     time rate.
!
      CALL SPKSSB  ( SVOBS,  ET, SVREF,         STOBS  )
      CALL SPKLTC  ( SVTARG, ET, SVREF, SVCORR, STOBS, &
                     STATE,  LT, DLT                   )
      T_GETDLT = DLT
      RETURN


      END





