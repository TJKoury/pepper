C$Procedure T_SPKCHB ( Test utility, fit SPK data with Chebys )
  
      SUBROUTINE T_SPKCHB ( TARGET, CENTER, FRAME, FIRST,  LAST,
     .                      NPOS,   NVEL,   WORK,  POSCOF, VELCOF )

C$ Abstract
C
C     Create Chebyshev expansions for the position and velocity
C     of an object whose ephemeris is given by SPK data.
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
C     CHEBYSHEV
C     FIT
C     POLYNOMIAL
C     SPK
C     TEST
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      INTEGER               MAXSIZ
      PARAMETER           ( MAXSIZ = 25 )

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         CENTER
      CHARACTER*(*)         FRAME
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      INTEGER               NPOS
      INTEGER               NVEL
      DOUBLE PRECISION      WORK   ( * )
      DOUBLE PRECISION      POSCOF ( NPOS, 3 )
      DOUBLE PRECISION      VELCOF ( NVEL, 3 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MAXSIZ     P   Maximum number of terms in expansion.
C     TARGET     I   Target body name.
C     CENTER     I   Center name.
C     FRAME      I   Frame name.
C     FIRST      I   Interval start time in seconds past J2000 TDB.
C     LAST       I   Interval stop time in seconds past J2000 TDB.
C     NPOS       I   Number of terms in position expansion.
C     NVEL       I   Number of terms in velocity expansion.
C     WORK      I-O  Workspace.
C     POSCOF     O   Chebyshev coefficients for position.
C     VELCOF     O   Chebyshev coefficients for velocity.
C   
C$ Detailed_Input
C
C     TARGET         is the name of the target body. Chebyshev
C                    coefficients for the state of the target relative
C                    to the center, in the reference frame specified by
C                    FRAME, are computed by this routine.
C
C     CENTER         is the name of the center of motion.
C
C     FRAME          is the name of the reference frame in which to
C                    states are to be computed.
C
C     FIRST,
C     LAST           are the time bounds of the time interval over
C                    which the Chebyshev expansion is applicable. Times
C                    are expressed as seconds past J2000 TDB.
C                
C     NPOS           is the number of terms in the position expansion.
C                    The degree of the expansion is NPOS-1. The maximum
C                    value of NPOS is limited by the parameter MAXSIZ.
C
C     NVEL           is the number of terms in the velocity expansion.
C                    The degree of the expansion is NVEL-1. The maximum
C                    value of NVEL is limited by the parameter MAXSIZ.
C
C     WORK           is a workspace array; this workspace is used by
C                    the Chebyshev fitter CHBFIT. The size of WORK must
C                    be at least
C
C                       MAX ( NPOS, NVEL )  +  1
C                 
C$ Detailed_Output
C
C     WORK           is the workspace array, modified by this
C                    routine.
C
C     POSCOF         is a Chebyshev expansion of degree NPOS-1 for the
C                    position of TARGET with respect to CENTER, in the
C                    reference frame FRAME, over the time interval
C                    [FIRST, LAST].
C
C
C     VELCOF         is the corresponding Chebyshev expansion
C                    for velocity. The degree is NVEL-1.
C
C$ Parameters
C
C     MAXSIZ is the maximum number of terms in either output
C     expansion. This parameter is declared in the support
C     library utility routine CHBFIT. 
C
C$ Exceptions
C
C     1) If either the input target or center name cannot be mapped
C        to an ID code, the error SPICE(NOTRANSLATION) is signaled.
C
C     2) If either NPOS or NVEL is outside of the range 1:MAXSIZ,
C        the error SPICE(INVALIDCOUNT) is signaled.
C
C     3) Any exceptions that may be incurred by SPKEZ may be incurred
C        by this routine.
C 
C$ Files
C
C     1) SPK data are required to provide the state of the target
C        relative to the center of motion over the time interval 
C        [FIRST, LAST].
C
C$ Particulars
C
C     This routine supports testing of Chebyshev-based SPK and PCK
C     code.
C       
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) The quality of the Chebyshev expansions produced by
C        this routine is dependent on the suitability of the
C        input data. Attempting to fit ephemerides over 
C        excessively long time intervals will result in 
C        useless outputs.
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
C-    TESTUTIL Version 1.0.0, 26-NOV-2013 (NJB)
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               RETURN

C
C     Other functions
C
      DOUBLE PRECISION      T_GETX
      DOUBLE PRECISION      T_GETY
      DOUBLE PRECISION      T_GETZ
      DOUBLE PRECISION      T_GETDX
      DOUBLE PRECISION      T_GETDY
      DOUBLE PRECISION      T_GETDZ
      DOUBLE PRECISION      T_STCINI

      EXTERNAL              T_GETX
      EXTERNAL              T_GETY
      EXTERNAL              T_GETZ
      EXTERNAL              T_GETDX
      EXTERNAL              T_GETDY
      EXTERNAL              T_GETDZ

C
C     Local parameters
C
 
C
C     Local variables
C
      DOUBLE PRECISION      RETVAL

      INTEGER               CENTID
      INTEGER               TARGID

      LOGICAL               FOUND

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SPKCHB' )


      CALL BODS2C ( TARGET, TARGID, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Target name # could not be translated to '
     .   //            'an ID code.'                              )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                     )
         CALL CHKOUT ( 'T_SPKCHB'                                 )
         RETURN

      END IF

      CALL BODS2C ( CENTER, CENTID, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Center name # could not be translated to '
     .   //            'an ID code.'                              )
         CALL ERRCH  ( '#', CENTER                                )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                     )
         CALL CHKOUT ( 'T_SPKCHB'                                 )
         RETURN

      END IF

      IF ( ( NPOS .LT. 1 ) .OR. ( NPOS .GT. MAXSIZ )  ) THEN

         CALL SETMSG ( 'Position coefficient count # must be in '
     .   //            'the range 1:# but was #.'                 )
         CALL ERRINT ( '#', MAXSIZ                                )
         CALL ERRINT ( '#', NPOS                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                      )
         CALL CHKOUT ( 'T_SPKCHB'                                 )
         RETURN

      END IF

      IF ( ( NVEL .LT. 1 ) .OR. ( NVEL .GT. MAXSIZ )  ) THEN

         CALL SETMSG ( 'Velocity coefficient count # must be in '
     .   //            'the range 1:# but was #.'                 )
         CALL ERRINT ( '#', MAXSIZ                                )
         CALL ERRINT ( '#', NVEL                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                      )
         CALL CHKOUT ( 'T_SPKCHB'                                 )
         RETURN

      END IF
C
C     Initialize the state component functions. The returned
C     value is not used; the called function operates by
C     side effects.
C
      RETVAL = T_STCINI ( TARGID, FRAME, 'NONE', CENTID )
C
C     Fit the state vector components.
C
      CALL CHBFIT ( T_GETX,  FIRST, LAST, NPOS, WORK, POSCOF(1,1) )
      CALL CHBFIT ( T_GETY,  FIRST, LAST, NPOS, WORK, POSCOF(1,2) )
      CALL CHBFIT ( T_GETZ,  FIRST, LAST, NPOS, WORK, POSCOF(1,3) )
      CALL CHBFIT ( T_GETDX, FIRST, LAST, NVEL, WORK, VELCOF(1,1) )
      CALL CHBFIT ( T_GETDY, FIRST, LAST, NVEL, WORK, VELCOF(1,2) )
      CALL CHBFIT ( T_GETDZ, FIRST, LAST, NVEL, WORK, VELCOF(1,3) )

      CALL CHKOUT ( 'T_SPKCHB' )
      RETURN
      END


