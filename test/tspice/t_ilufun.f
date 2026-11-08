C$Procedure T_ILUFUN ( TSPICE illumination angle functions )

      DOUBLE PRECISION FUNCTION T_ILUFUN ( METHOD, TARGET, ILLUM,  
     .                                     ET,     FIXREF, ABCORR,
     .                                     OBSRVR, SPOINT, NORMAL )
  
C$ Abstract
C
C     This is an umbrella routine for functions that return
C     illumination angles.
C
C     This is a TSPICE utility that supports F_ZZILUSTA.
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
C     GF
C     PCK
C     TIME
C     SPK
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         METHOD
      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLUM
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )

C$ Brief_I/O
C
C     Variable  I/O  Entry point
C     --------  ---  --------------------------------------------------
C     METHOD     I   T_ILUINI
C     TARGET     I   T_ILUINI
C     ILLUM      I   T_ILUINI
C     ET         I   T_ILUPHS, T_ILUINC, T_ILUEMI
C     FIXREF     I   T_ILUINI
C     ABCORR     I   T_ILUINI
C     OBSRVR     I   T_ILUINI
C     SPOINT     I   T_ILUINI
C     NORMAL     I   T_ILUINI
C
C$ Detailed_Input
C                                 
C     See the headers of the entry points.
C
C$ Detailed_Output
C
C     See the headers of the entry points.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If this routine is called directly, the error
C         SPICE(BOGUSENTRY) is signaled.
C
C$ Files
C
C     See ILLUMG.
C
C$ Particulars
C
C     This is a specialized routine intended for use only
C     by F_ZZILUSTA.
C
C     This suite of routines is intended to be used as follows:
C
C        1) The calling test program should first call
C
C              T_ILUINI
C
C           to store the inputs, other than time, that will
C           be used to compute illumination angles.
C     
C        2) Call any of the entry points
C
C              T_ILUPHS
C              T_ILUINC
C              T_ILUEMI
C
C           to compute illumination angles at a time of interest. There
C           is no need to call T_ILUINI between calls to these routines
C           unless any of the inputs, other than time, have changed.
C     
C$ Examples
C
C     See usage in F_ZZILUSTA.
C
C$ Restrictions
C
C     1) This routine is intended for use only by the GF subsystem.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
C
C-&
 
C
C     Entry points
C
      DOUBLE PRECISION      T_ILUINI
      DOUBLE PRECISION      T_ILUPHS
      DOUBLE PRECISION      T_ILUINC
      DOUBLE PRECISION      T_ILUEMI

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               CORLEN
      PARAMETER           ( CORLEN = 10 )

      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 50 )

C
C     Local variables
C     
      CHARACTER*(CORLEN)    SVCORR
      CHARACTER*(BDNMLN)    SVILUM
      CHARACTER*(MTHLEN)    SVMETH
      CHARACTER*(BDNMLN)    SVOBS
      CHARACTER*(FRNMLN)    SVREF
      CHARACTER*(BDNMLN)    SVTARG

      DOUBLE PRECISION      LOCEMI ( 2 )
      DOUBLE PRECISION      LOCINC ( 2 )
      DOUBLE PRECISION      LOCPHS ( 2 )
      DOUBLE PRECISION      SVNRML ( 3 )
      DOUBLE PRECISION      SVPNT  ( 3 )

C
C     Saved variables
C
      SAVE                  SVCORR
      SAVE                  SVILUM
      SAVE                  SVMETH
      SAVE                  SVNRML
      SAVE                  SVOBS
      SAVE                  SVPNT
      SAVE                  SVREF
      SAVE                  SVTARG


      CALL CHKIN  ( 'T_ILUFUN'          )

      T_ILUFUN = 0.D0

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_ILUFUN'          )
      RETURN



C$Procedure T_ILUINI ( TSPICE: initialize entry points. )

      ENTRY T_ILUINI ( METHOD, TARGET, ILLUM,  FIXREF,
     .                 ABCORR, OBSRVR, SPOINT, NORMAL )
  
C$ Abstract
C
C     Initialize the illumination angle functions in this 
C     suite of test routines: store inputs to ZZILUMIN.
C
C     This is a TSPICE utility that supports F_ZZILUSTA.
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
C     GF
C     PCK
C     TIME
C     SPK
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     SEARCH
C     UTILITY
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         METHOD
C     CHARACTER*(*)         TARGET
C     CHARACTER*(*)         ILLUM
C     CHARACTER*(*)         FIXREF
C     CHARACTER*(*)         ABCORR
C     CHARACTER*(*)         OBSRVR
C     DOUBLE PRECISION      SPOINT ( 3 )
C     DOUBLE PRECISION      NORMAL ( 3 )
C
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     METHOD     I   Computation method.
C     TARGET     I   Name of target body associated with surface point.
C     ILLUM      I   Name of illumination source.
C     FIXREF     I   Body-fixed reference frame.
C     ABCORR     I   Aberration correction.
C     OBSRVR     I   Name of observer.
C     SPOINT     I   Surface point.
C     NORMAL     I   Outward normal vector at surface point.
C
C$ Detailed_Input
C                                 
C     METHOD         is a string specifying the computation method to
C                    be used by this routine. The only value currently
C                    allowed is 'ELLIPSOID'. This indicates that the
C                    target body shape is modeled as an ellipsoid.
C
C                    Case and leading and trailing blanks are not
C                    significant in METHOD.
C
C     TARGET         is the name of the target body associated with the
C                    surface point SPOINT. TARGET may be a body name or
C                    an ID code provided as as string.
C
C     ILLUM          is the name of the illumination source used to
C                    define the illumination angles computed by this
C                    routine. ILLUM may be a body name or an ID code
C                    provided as as string.
C
C     FIXREF         is the name of the body-centered, body-fixed
C                    reference frame relative to which SPOINT is
C                    specified. The frame's center must coincide with
C                    TARGET.
C
C     ABCORR         indicates the aberration corrections to be
C                    applied. Only reception corrections are supported.
C                    See the header of ILUMIN for a discussion of
C                    aberration corrections used in illumination angle
C                    computations.
C
C     OBSRVR         is the name of the observing body. OBSRVR may be a
C                    body name or an ID code provided as as string.
C
C     SPOINT         is a 3-vector containing the cartesian coordinates
C                    of the surface point at which the illumination
C                    angle rates are to be computed. SPOINT is
C                    expressed in the body-centered, body-fixed frame
C                    designated by FIXREF (see description above).
C
C                    Units are km.
C
C     NORMAL         is an outward normal vector to be used for
C                    emission angle and illumination source incidence
C                    angle calculations. NORMAL should be orthogonal
C                    at SPOINT to the target body's surface.
C                       
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     See ILLUMG.
C
C$ Particulars
C
C     This routine stores inputs required by ZZILUSTA. The stored inputs
C     are used by the other entry points to compute illumination angles
C     a specified times.
C     
C$ Examples
C
C     See usage in T_DZZILU.
C
C$ Restrictions
C
C     1) This is a specialized routine intended for use only
C         by F_ZZILUSTA.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
C
C-&
 
C
C     Give this function a return value.
C
      T_ILUINI = 0.D0

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'T_ILUINI' )

      SVMETH = METHOD
      SVTARG = TARGET
      SVILUM = ILLUM
      SVREF  = FIXREF
      SVCORR = ABCORR
      SVOBS  = OBSRVR

      CALL VEQU ( SPOINT, SVPNT  )
      CALL VEQU ( NORMAL, SVNRML )

      CALL CHKOUT ( 'T_ILUINI' )
      RETURN



C$Procedure T_ILUPHS ( TSPICE: Compute phase angle )
  
      ENTRY T_ILUPHS ( ET )

C$ Abstract
C
C     Compute the phase angle at ET.
C
C     This is a TSPICE utility that supports F_ZZILUSTA.
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
C     GF
C     PCK
C     TIME
C     SPK
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     SEARCH
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
C     ET         I   Epoch at which to compute phase angle.
C
C     The function returns the phase angle at ET defined by 
C     the stored inputs.
C
C$ Detailed_Input
C                                 
C     ET             is the epoch at which a phase angle is
C                    to be computed. ET is expressed as seconds
C                    past J2000 TDB.
C                       
C$ Detailed_Output
C
C     The function returns the phase angle at ET defined by 
C     the inputs saved by the last call to T_ILUINI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See the Exceptions section of the header of ZZILUSTA.
C
C$ Files
C
C     See ILLUMG.
C
C$ Particulars
C
C     This routine uses ZZILUSTA to compute a specified phase angle
C     at a given time. The inputs other than time are saved values
C     stored by the last call to T_ILUINI.
C
C     See ILLUMG for a description of this angle.
C     
C$ Examples
C
C     See usage in T_DZZILU.
C
C$ Restrictions
C
C     1) This is a specialized routine intended for use only
C         by F_ZZILUSTA.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
C
C-&

      T_ILUPHS = 0.D0

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ILUPHS' )
      
      CALL ZZILUSTA ( SVMETH, SVTARG, SVILUM, ET,
     .                SVREF,  SVCORR, SVOBS,  SVPNT,
     .                SVNRML, LOCPHS, LOCINC, LOCEMI  )

      T_ILUPHS = LOCPHS(1)
      
      CALL CHKOUT ( 'T_ILUPHS' )
      RETURN




C$Procedure T_ILUINC ( TSPICE: Compute incidence angle )
  
      ENTRY T_ILUINC ( ET )

C$ Abstract
C
C     Compute the illumination source incidence angle at ET.
C
C     This is a TSPICE utility that supports F_ZZILUSTA.
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
C     GF
C     PCK
C     TIME
C     SPK
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     SEARCH
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
C     ET         I   Epoch at which to compute incidence angle.
C
C     The function returns the illumination source angle at ET defined
C     by the stored inputs.
C
C$ Detailed_Input
C                                 
C     ET             is the epoch at which the illumination source
C                    incidence angle is to be computed. ET is expressed
C                    as seconds past J2000 TDB.
C                       
C$ Detailed_Output
C
C     The function returns the illumination source incidence angle at
C     ET defined by the inputs saved by the last call to T_ILUINI.
C     
C     See ILLUMG for a description of this angle.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See the Exceptions section of the header of ZZILUSTA.
C
C$ Files
C
C     See ILLUMG.
C
C$ Particulars
C
C     This routine uses ZZILUSTA to compute a specified illumination
C     source incidence angle at a given time. The inputs other than
C     time are saved values stored by the last call to T_ILUINI.
C     
C$ Examples
C
C     See usage in T_DZZILU.
C
C$ Restrictions
C
C     1) This is a specialized routine intended for use only
C         by F_ZZILUSTA.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
C
C-&

      T_ILUINC = 0.D0

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ILUINC' )

      CALL ZZILUSTA ( SVMETH, SVTARG, SVILUM, ET,
     .                SVREF,  SVCORR, SVOBS,  SVPNT,
     .                SVNRML, LOCPHS, LOCINC, LOCEMI  )

      T_ILUINC = LOCINC(1)
      
      CALL CHKOUT ( 'T_ILUINC' )
      RETURN




C$Procedure T_ILUEMI ( TSPICE: Compute emission angle )
  
      ENTRY T_ILUEMI ( ET )

C$ Abstract
C
C     Compute the emission angle at ET.
C
C     This is a TSPICE utility that supports F_ZZILUSTA.
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
C     GF
C     PCK
C     TIME
C     SPK
C
C$ Keywords
C
C     ANGLE
C     GEOMETRY
C     SEARCH
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
C     ET         I   Epoch at which to compute emission angle.
C
C     The function returns the emission angle at ET defined
C     by the stored inputs.
C
C$ Detailed_Input
C                                 
C     ET             is the epoch at which the emission angle is
C                    to be computed. ET is expressed as seconds
C                    past J2000 TDB.
C                       
C$ Detailed_Output
C
C     The function returns the emission angle at ET defined by the
C     inputs saved by the last call to T_ILUINI.
C     
C     See ILLUMG for a description of this angle.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     See the Exceptions section of the header of ZZILUSTA.
C
C$ Files
C
C     See ILLUMG.
C
C$ Particulars
C
C     This routine uses ZZILUSTA to compute a specified emission angle
C     at a given time. The inputs other than time are saved values
C     stored by the last call to T_ILUINI.
C     
C$ Examples
C
C     See usage in T_DZZILU.
C
C$ Restrictions
C
C     1) This is a specialized routine intended for use only
C         by F_ZZILUSTA.
C     
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 06-APR-2012 (NJB)
C
C-&

      T_ILUEMI = 0.D0


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_ILUEMI' )

      CALL ZZILUSTA ( SVMETH, SVTARG, SVILUM, ET,
     .                SVREF,  SVCORR, SVOBS,  SVPNT,
     .                SVNRML, LOCPHS, LOCINC, LOCEMI  )

      T_ILUEMI = LOCEMI(1)
      
      CALL CHKOUT ( 'T_ILUEMI' )
      RETURN

      END
