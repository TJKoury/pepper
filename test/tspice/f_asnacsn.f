C$Procedure      F_ASNACSN (Family of tests for DASINE and DACOSN)

      SUBROUTINE F_ASNACSN ( OK )

      IMPLICIT              NONE

      LOGICAL               OK

C$ Abstract
C
C     This is a test routine for the arc sine and arc cosine 
C     functions with argument bound checking.
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
C$ Version
C
C     TSPICE Version 1.0.0 7-MAR-2006 (LSE)
C-&
 
 
C
C     Test Utility Functions
C
 
 
C
C     SPICELIB functions
C

      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      DACOSN
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      HALFPI

C 
C     Local variables.
C

      DOUBLE PRECISION      TSTARG
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TSTANG

C
C     Begin every test family with an open call. 
C

      CALL TOPEN ( 'F_ASNACSN' )

C
C     Test for tolerance negative error.
C

      TOL       =  -1.D-12

      CALL TCASE  ( 'Tolerance negative, error in DASINE' )

      TSTANG    =   DASINE ( 0.D0,  TOL                   ) 

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK  ) 

      CALL TCASE  ( 'Tolerance negative, error in DACOSN' )

      TSTANG    =   DACOSN ( 1.D0,  TOL                   ) 

      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK  ) 

C
C     Test for error when argument is too large
C

      TOL       =  1.D-12

      TSTARG    =  1.D0 + 1.D-11

      CALL TCASE  ( 'Argument too large in DASINE.'               )

      TSTANG    =  DASINE ( TSTARG,  TOL                          ) 

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         ) 

      CALL TCASE  ( 'Is value = ASIN (1.0) in DASINE?)'           )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=', HALFPI(),  0.D0,  OK )

      CALL TCASE  ( 'Argument too large in DACOSN.'               )

      TSTANG    =  DACOSN ( TSTARG,  TOL                          ) 

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         ) 

      CALL TCASE  ( 'Is value = ACOS (1.0) in DACOSN?)'           )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=', 0.D0,  0.D0,  OK     ) 

C
C     Test for error when argument is too small
C

      TOL       =  1.D-12

      TSTARG    = -1.D0 - 1.D-11

      CALL TCASE  ( 'Argument too small in DASINE.'               )

      TSTANG    =  DASINE ( TSTARG,  TOL                          ) 

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         ) 

      CALL TCASE  ( 'Is value = ASIN (-1.0) in DASINE?)'          )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=',-HALFPI(),  0.D0,  OK )

      CALL TCASE  ( 'Argument too small in DACOSN.'               )

      TSTANG    =  DACOSN ( TSTARG,  TOL                          ) 

      CALL CHCKXC ( .TRUE., 'SPICE(INPUTOUTOFBOUNDS)', OK         ) 

      CALL TCASE  ( 'Is value = ACOS (-1.0) in DACOSN?)'          )

      CALL CHCKSD ( 'TSTANG', TSTANG,   '=', PI(),  0.D0,  OK     )

C
C     All done. 
C

      CALL T_SUCCESS ( OK )
      RETURN
      END

