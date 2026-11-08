C$Procedure T_TRAJFN ( Test utility, evaluate trajectory functions )
  
      DOUBLE PRECISION FUNCTION T_TRAJFN ( TARG, ET, REF, ABCORR, OBS )
      IMPLICIT NONE

C$ Abstract
C
C     Umbrella routine for function entry points that return state
C     components, light time and light time rate, and stellar 
C     aberration correction components and their rates.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations

      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      INTEGER               OBS

      INCLUDE 'zzabcorr.inc'
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body ID code.
C     ET         I   Epoch.
C     REF        I   Reference frame name.
C     ABCORR     I   Aberration correction.
C     OBS        I   Observer ID code.
C
C$ Detailed_Input
C
C     TARG           is the NAIF ID code of the target body. 
C
C     ET             is the time at which to evaluate a 
C                    state component, light time or light time
C                    rate, or stellar aberration correction
C                    component. ET is expressed as seconds
C                    past J2000 TDB.
C
C     REF            is the reference frame in which to 
C                    compute a state, aberration correction,
C                    or aberration correction rate.
C
C     ABCORR         is an aberration correction specification
C                    string. Any value acceptable to SPKEZ
C                    may be used.
C
C     OBS            is the NAIF ID of the observer body.
C
C$ Detailed_Output
C
C     The outputs of this routine are the values returned by this
C     routine's function entry points. See those entry points
C     for details.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by SPKEZ may be incurred
C        by this routine.
C 
C     2) Any exceptions that may be incurred by SPKLTC may be incurred
C        by this routine.
C
C     3) Any exceptions that may be incurred by the test utility
C        T_GETSA may be incurred by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine contains 15 function entry points which allow a
C     caller to fetch state components, stellar aberration correction
C     components, and light time and light time rate values via
C     function calls. These entry points are suitable for use with the
C     SUPPORT library Chebyshev fitting routine CHBFIT.
C
C     To use this package, an application should call the entry point
C
C        T_STCINI
C
C     to set the saved variables defining a state computation:
C
C        observer
C        target 
C        aberration correction
C        reference frame.
C
C     After this, any of the state or aberration correction computation
C     functions can be called with an ET (seconds past J2000 TDB) input
C     value.
C        
C     The entry points and their descriptions are listed below.
C 
C
C     T_GETDLT {Get light time derivative}
C     T_GETDSX {Get stellar aberration correction velocity X-component}
C     T_GETDSY {Get stellar aberration correction velocity Y-component}
C     T_GETDSZ {Get stellar aberration correction velocity Z-component}
C     T_GETDX  {Get velocity X-component}
C     T_GETDY  {Get velocity Y-component}
C     T_GETDZ  {Get velocity Z-component}
C     T_GETLT  {Get light time}
C     T_GETSX  {Get stellar aberration correction X-component}
C     T_GETSY  {Get stellar aberration correction Y-component}
C     T_GETSZ  {Get stellar aberration correction Z-component}
C     T_GETX   {Get position X-component}
C     T_GETY   {Get position Y-component}
C     T_GETZ   {Get position Z-component}
C     T_STCINI {Initialize state and aberration correction computations}
C
C
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     Due to the large number and trivial nature of the entry points,
C     these functions have very abbreviated header comments.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 26-DEC-2007 (NJB)
C
C-&
 

C
C     Entry points
C
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


C
C     Local parameters
C
      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

C
C     Local variables
C      
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
 
    
C
C     Saved variables
C
      SAVE


      T_TRAJFN = 0.D0

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      RETURN 




C$Procedure T_STCINI ( Test utility, state computation initialization )
  
      ENTRY T_STCINI ( TARG, REF, ABCORR, OBS )

C$ Abstract
C
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     INTEGER               TARG
C     CHARACTER*(*)         REF
C     CHARACTER*(*)         ABCORR
C     INTEGER               OBS
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TARG       I   Target body ID code.
C     REF        I   Reference frame name.
C     ABCORR     I   Aberration correction.
C     OBS        I   Observer ID code.
C
C$ Detailed_Input
C
C     TARG           is the NAIF ID code of the target body. 
C
C     REF            is the reference frame in which to 
C                    compute a state, aberration correction,
C                    or aberration correction rate.
C
C     ABCORR         is an aberration correction specification
C                    string. Any value acceptable to SPKEZ
C                    may be used.
C
C     OBS            is the NAIF ID of the observer body.
C
C$ Detailed_Output
C
C     This routine operates by side effects. It returns an
C     unspecfied double precision constant, since Fortran
C     requires all functions and function entry points to 
C     return values.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by ZZPRSCOR may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     This function initializes a state, aberration correction, or
C     aberration correction rate computation by saving parameters,
C     other than time, required to define a state. This function should
C     be called prior to calling the other entry points.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 30-DEC-2007 (NJB)
C
C-&
 

C
C     Set the double precision return value, since one is
C     required.
C     
      T_STCINI = 0.D0

C
C     Save the input arguments for later use.
C
      SVTARG = TARG
      SVREF  = REF
      SVCORR = ABCORR
      SVOBS  = OBS


      RETURN


   

C$Procedure T_GETX ( Test utility, get position X component )
  
      ENTRY T_GETX ( ET )

C$ Abstract
C
C     Return X component of position vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    whose X position component is requested. ET is
C                    expressed as seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This routine returns the X component of the position vector
C     defined by the input ET value and the saved observer, target,
C     reference frame, and aberration correction values initialized
C     by the last call to T_STCINI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by SPKEZ may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     None.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the state. Return the X component of position.
C
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETX = STATE(1)

      RETURN





C$Procedure T_GETY ( Test utility, get position Y component )
  
      ENTRY T_GETY ( ET )

C$ Abstract
C
C     Return Y component of position vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    whose Y position component is requested. ET is
C                    expressed as seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This routine returns the Y component of the position vector
C     defined by the input ET value and the saved observer, target,
C     reference frame, and aberration correction values initialized
C     by the last call to T_STCINI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by SPKEZ may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     None.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the state. Return the Y component of position.
C
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETY = STATE(2)

      RETURN



C$Procedure T_GETZ ( Test utility, get position Z component )
  
      ENTRY T_GETZ ( ET )

C$ Abstract
C
C     Return Z component of position vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    whose Z position component is requested. ET is
C                    expressed as seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This routine returns the Z component of the position vector
C     defined by the input ET value and the saved observer, target,
C     reference frame, and aberration correction values initialized
C     by the last call to T_STCINI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by SPKEZ may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     None.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the state. Return the Z component of position.
C
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETZ = STATE(3)

      RETURN




C$Procedure T_GETDX ( Test utility, get velocity X component )
  
      ENTRY T_GETDX ( ET )

C$ Abstract
C
C     Return X component of velocity vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    whose X velocity component is requested. ET is
C                    expressed as seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This routine returns the X component of the velocity vector
C     defined by the input ET value and the saved observer, target,
C     reference frame, and aberration correction values initialized
C     by the last call to T_STCINI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by SPKEZ may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     None.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the state. Return the X component of velocity.
C 
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETDX = STATE(4)
 
      RETURN



C$Procedure T_GETDY ( Test utility, get velocity Y component )
  
      ENTRY T_GETDY ( ET )

C$ Abstract
C
C     Return Y component of velocity vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    whose Y velocity component is requested. ET is
C                    expressed as seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This routine returns the Y component of the velocity vector
C     defined by the input ET value and the saved observer, target,
C     reference frame, and aberration correction values initialized
C     by the last call to T_STCINI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by SPKEZ may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     None.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the state. Return the Y component of velocity.
C
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETDY = STATE(5)

      RETURN



C$Procedure T_GETDZ ( Test utility, get velocity Z component )
  
      ENTRY T_GETDZ ( ET )

C$ Abstract
C
C     Return Z component of velocity vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    whose Z velocity component is requested. ET is
C                    expressed as seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This routine returns the Z component of the velocity vector
C     defined by the input ET value and the saved observer, target,
C     reference frame, and aberration correction values initialized
C     by the last call to T_STCINI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by SPKEZ may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     None.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the state. Return the Z component of velocity.
C
      CALL SPKEZ ( SVTARG, ET, SVREF, SVCORR, SVOBS, STATE, LT )
      T_GETDZ = STATE(6)

      RETURN




C$Procedure T_GETSX ( Test utility, get S.A. position X component )
  
      ENTRY T_GETSX ( ET )

C$ Abstract
C
C     Return X component of the stellar aberration correction
C     position vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    with which the specified stellar aberration
C                    correction is associated. ET is expressed as
C                    seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This function returns the X component of the stellar aberration
C     correction position vector of the state defined by the input time
C     ET and the parameters saved from the last call to the
C     initialization entry point T_STCINI.
C
C     The stellar aberration correction position vector is the
C     difference between the geometric or light time corrected position
C     computed by SPKTLC for the given inputs, and this position after
C     the stellar aberration correction has been applied.
C
C     Note that if the saved aberration correction is NONE, than the
C     stellar aberration correction will be applied to the geometric
C     state vector computed by SPKEZ.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by T_GETSA may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     See the test utility T_GETSA for details of how the stellar
C     aberration correction position vector is computed.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the stellar aberration correction offset PCORR and its
C     derivative with respect to time DPCORR. Return the X component of
C     the stellar aberration correction position offset.
C
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETSX = PCORR(1)
 
      RETURN



C$Procedure T_GETSY ( Test utility, get S.A. position Y component )
  
      ENTRY T_GETSY ( ET )

C$ Abstract
C
C     Return Y component of the stellar aberration correction
C     position vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    with which the specified stellar aberration
C                    correction is associated. ET is expressed as
C                    seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This function returns the Y component of the stellar aberration
C     correction position vector of the state defined by the input time
C     ET and the parameters saved from the last call to the
C     initialization entry point T_STCINI.
C
C     The stellar aberration correction position vector is the
C     difference between the geometric or light time corrected position
C     computed by SPKTLC for the given inputs, and this position after
C     the stellar aberration correction has been applied.
C
C     Note that if the saved aberration correction is NONE, than the
C     stellar aberration correction will be applied to the geometric
C     state vector computed by SPKEZ.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by T_GETSA may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     See the test utility T_GETSA for details of how the stellar
C     aberration correction position vector is computed.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the stellar aberration correction offset PCORR and its
C     derivative with respect to time DPCORR. Return the Y component of
C     the stellar aberration correction position offset.
C
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETSY = PCORR(2)
      RETURN



C$Procedure T_GETSZ ( Test utility, get S.A. position Z component )
  
      ENTRY T_GETSZ ( ET )

C$ Abstract
C
C     Return Z component of the stellar aberration correction
C     position vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    with which the specified stellar aberration
C                    correction is associated. ET is expressed as
C                    seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This function returns the Z component of the stellar aberration
C     correction position vector of the state defined by the input time
C     ET and the parameters saved from the last call to the
C     initialization entry point T_STCINI.
C
C     The stellar aberration correction position vector is the
C     difference between the geometric or light time corrected position
C     computed by SPKTLC for the given inputs, and this position after
C     the stellar aberration correction has been applied.
C
C     Note that if the saved aberration correction is NONE, than the
C     stellar aberration correction will be applied to the geometric
C     state vector computed by SPKEZ.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by T_GETSA may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     See the test utility T_GETSA for details of how the stellar
C     aberration correction position vector is computed.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the stellar aberration correction offset PCORR and its
C     derivative with respect to time DPCORR. Return the Z component of
C     the stellar aberration correction position offset.
C
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETSZ = PCORR(3)

      RETURN




C$Procedure T_GETDSX ( Test utility, get S.A. velocity X component )
  
      ENTRY T_GETDSX ( ET )

C$ Abstract
C
C     Return X component of the stellar aberration correction
C     velocity vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    with which the specified stellar aberration
C                    correction is associated. ET is expressed as
C                    seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This function returns the X component of the stellar aberration
C     correction velocity vector of the state defined by the input time
C     ET and the parameters saved from the last call to the
C     initialization entry point T_STCINI.
C
C     The stellar aberration correction position vector is the
C     difference between the geometric or light time corrected position
C     computed by SPKTLC for the given inputs, and this position after
C     the stellar aberration correction has been applied. The stellar
C     aberration correction velocity vector is simply the derivative
C     with respect to time (TDB) of this position vector.
C
C     Note that if the saved aberration correction is NONE, than the
C     stellar aberration correction will be applied to the geometric
C     state vector computed by SPKEZ.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by T_GETSA may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     See the test utility T_GETSA for details of how the stellar
C     aberration correction velocity vector is computed.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the stellar aberration correction offset PCORR and its
C     derivative with respect to time DPCORR. Return the X component of
C     the stellar aberration correction velocity offset.
C
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETDSX = DPCORR(1)

      RETURN




C$Procedure T_GETDSY ( Test utility, get S.A. velocity Y component )
  
      ENTRY T_GETDSY ( ET )

C$ Abstract
C
C     Return Y component of the stellar aberration correction
C     velocity vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    with which the specified stellar aberration
C                    correction is associated. ET is expressed as
C                    seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This function returns the Y component of the stellar aberration
C     correction velocity vector of the state defined by the input time
C     ET and the parameters saved from the last call to the
C     initialization entry point T_STCINI.
C
C     The stellar aberration correction position vector is the
C     difference between the geometric or light time corrected position
C     computed by SPKTLC for the given inputs, and this position after
C     the stellar aberration correction has been applied. The stellar
C     aberration correction velocity vector is simply the derivative
C     with respect to time (TDB) of this position vector.
C
C     Note that if the saved aberration correction is NONE, than the
C     stellar aberration correction will be applied to the geometric
C     state vector computed by SPKEZ.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by T_GETSA may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     See the test utility T_GETSA for details of how the stellar
C     aberration correction velocity vector is computed.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&

C
C     Look up the stellar aberration correction offset PCORR and its
C     derivative with respect to time DPCORR. Return the Y component of
C     the stellar aberration correction velocity offset.
C
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETDSY = DPCORR(2)

      RETURN




C$Procedure T_GETDSZ ( Test utility, get S.A. velocity Z component )
  
      ENTRY T_GETDSZ ( ET )

C$ Abstract
C
C     Return Z component of the stellar aberration correction
C     velocity vector at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    with which the specified stellar aberration
C                    correction is associated. ET is expressed as
C                    seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This function returns the Z component of the stellar aberration
C     correction velocity vector of the state defined by the input time
C     ET and the parameters saved from the last call to the
C     initialization entry point T_STCINI.
C
C     The stellar aberration correction position vector is the
C     difference between the geometric or light time corrected position
C     computed by SPKTLC for the given inputs, and this position after
C     the stellar aberration correction has been applied. The stellar
C     aberration correction velocity vector is simply the derivative
C     with respect to time (TDB) of this position vector.
C
C     Note that if the saved aberration correction is NONE, than the
C     stellar aberration correction will be applied to the geometric
C     state vector computed by SPKEZ.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by T_GETSA may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     See the test utility T_GETSA for details of how the stellar
C     aberration correction velocity vector is computed.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&

C
C     Look up the stellar aberration correction offset PCORR and its
C     derivative with respect to time DPCORR. Return the Z component of
C     the stellar aberration correction velocity offset.
C
      CALL T_GETSA ( SVTARG, ET, SVREF, SVCORR, SVOBS, PCORR, DPCORR )
      T_GETDSZ = DPCORR(3)

      RETURN




C$Procedure T_GETLT ( Test utility, get light time )
  
      ENTRY T_GETLT ( ET )

C$ Abstract
C
C     Return observer-target light time at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    with which the requested light time is associated.
C                    ET is expressed as seconds past J2000 TDB.
C
C$ Detailed_Output
C
C     This routine returns the one-way light time defined by the input
C     ET value and the saved observer, target, reference frame, and
C     aberration correction values initialized by the last call to
C     T_STCINI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by SPKLTC may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     None.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the light time corrected state, along with
C     the light time and the light time rate. Return the light 
C     time.
C
      CALL SPKSSB ( SVOBS,  ET, SVREF,         STOBS  )
      CALL SPKLTC ( SVTARG, ET, SVREF, SVCORR, STOBS, 
     .              STATE,  LT, DLT                   )
      T_GETLT =  LT

      RETURN





C$Procedure T_GETDLT ( Test utility, get light time rate )
  
      ENTRY T_GETDLT ( ET )

C$ Abstract
C
C     Return observer-target light time rate at ET.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     DOUBLE PRECISION      ET
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Epoch.
C
C$ Detailed_Input
C
C     ET             is the time at which to evaluate the state vector
C                    with which the requested light time rate is
C                    associated. ET is expressed as seconds past J2000
C                    TDB.
C
C$ Detailed_Output
C
C     This function returns the rate---that is, the derivative with
C     respect to time (TDB)---of one way light time associated with the
C     state defined by the input time ET and the parameters saved from
C     the last call to the initialization entry point T_STCINI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) Any exceptions that may be incurred by SPKLTC may be incurred
C        by this routine.
C
C$ Files
C
C     See the header of SPKEZ.
C
C$ Particulars
C
C     The terms "rate" and "derivative with respect to time" are
C     synonyms.
C       
C$ Examples
C
C     See the test family F_ABCORR for usage examples.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 21-DEC-2007 (NJB)
C
C-&
 
C
C     Look up the light time corrected state, along with
C     the light time and the light time rate. Return the light 
C     time rate.
C
      CALL SPKSSB  ( SVOBS,  ET, SVREF,         STOBS  ) 
      CALL SPKLTC  ( SVTARG, ET, SVREF, SVCORR, STOBS,
     .               STATE,  LT, DLT                   )
      T_GETDLT = DLT
      RETURN


      END





