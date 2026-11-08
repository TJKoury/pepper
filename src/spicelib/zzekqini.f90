!$Procedure   ZZEKQINI ( Private: EK, initialize encoded query )

      SUBROUTINE ZZEKQINI ( ISIZE, DSIZE, EQRYI, EQRYC, EQRYD )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Initialize encoded EK query.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      INCLUDE 'ekquery.inc'
      INCLUDE 'ekqlimit.inc'
      INCLUDE 'ekbool.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               ISIZE
      INTEGER               DSIZE
      INTEGER               EQRYI ( LBCELL : * )
      CHARACTER*(*)         EQRYC
      DOUBLE PRECISION      EQRYD ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ISIZE      I   Size of integer component of encoded query.
!     DSIZE      I   Size of d.p. component of encoded query.
!     EQRYI     I-O  Integer component of query.
!     EQRYC     I-O  Character component of query.
!     EQRYD     I-O  D.p. component of query.
!
! $ Detailed_Input
!
!     ISIZE          is the size of the cell comprising the integer
!                    component of the encoded query.
!
!     DSIZE          is the size of the array comprising the double
!                    precision component of the encoded query.
!
!     EQRYI          is an integer array that is to serve as the
!                    integer portion of an encoded EK query.  EQRYI
!                    will be initialized as an integer cell having
!                    size ISIZE.
!
!     EQRYC          is a character string that is to serve as
!                    the character portion of an encoded EK query.
!                    EQRYC will be set to blank.
!
!     EQRYD          is a double precision array that is to serve as
!                    the numeric portion of an encoded EK query.
!
! $ Detailed_Output
!
!     EQRYI,
!     EQRYD,
!     EQRYC          are the components of an initialized EK query.
!                    The query's architecture code will be set to 1.
!                    All counts will be set to zero.  The array
!                    EQRYD will be zeroed out.  The character string
!                    EQRYC will be blank.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) ISIZE must be large enough to accommodate a query with the
!        maximum number of tables, constraints, and tokens, and
!        indicated by the parameters in ekqlimit.inc.  If ISIZE is
!        too small, the error SPICE(CELLTOOSMALL) will be signaled.
!
!     2) DSIZE must be large enough to accommodate the largest number
!        of numeric tokens that can occur in a query.  If DSIZE is
!        too small, the error SPICE(CELLTOOSMALL) will be signaled.
!
!     3) EQRYC must be long enough to accommodate all of the character
!        data that can occur in a query.  If EQRYC is too short, the
!        error  SPICE(STRINGTOOSHORT) will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine assumes that encoded EK query architecture version
!     1 is to be used with the query to be initialized; this routine
!     will not work with any other architecture version.
!
! $ Examples
!
!     See ZZEKPARS.
!
! $ Restrictions
!
!     1) This routine is private to the EK library.  No routines
!        external to the EK library should call this routine.
!
!     2) Uses EK encoded query architecture version 1.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local parameters
!
!
!     Minimum upper bound for the integer cell of an encoded query:
!
      INTEGER               MNISIZ
      PARAMETER           ( MNISIZ =     EQVBAS &
                                      +  MAXTAB * EQVDSZ * 2 &
                                      +  MAXCON * EQCDSZ &
                                      +  MAXCON &
                                      +  MAXORD * EQODSZ &
                                      +  MAXSEL * EQSDSZ )

!
!     Local variables
!
      INTEGER               I


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKQINI' )
      END IF


!
!     Check sizes:
!
      IF ( ISIZE .LT. MNISIZ ) THEN

         CALL SETMSG ( 'Size of integer component of encoded query ' // &
                       'is #; at least # elements are required.'      )
         CALL ERRINT ( '#',   ISIZE                                   )
         CALL ERRINT ( '#',   MNISIZ                                  )
         CALL SIGERR ( 'SPICE(CELLTOOSMALL)'                          )
         CALL CHKOUT ('ZZEKQINI'                                      )
         RETURN

      END IF


      IF ( DSIZE .LT. MAXQNM ) THEN

         CALL SETMSG ( 'Size of d.p. component of encoded query ' // &
                       'is #; at least # elements are required.'      )
         CALL ERRINT ( '#',   DSIZE                                   )
         CALL ERRINT ( '#',   MAXQNM                                  )
         CALL SIGERR ( 'SPICE(CELLTOOSMALL)'                          )
         CALL CHKOUT ('ZZEKQINI'                                      )
         RETURN

      END IF


      IF ( LEN(EQRYC) .LT. MAXCLN ) THEN

         CALL SETMSG ( 'Size of character component of encoded query '// &
                       'is #; a length of at least # characters is '  // &
                       'required.'                                    )
         CALL ERRINT ( '#',   LEN(EQRYC)                              )
         CALL ERRINT ( '#',   MAXCLN                                  )
         CALL SIGERR ( 'SPICE(STRINGTOOSHORT)'                        )
         CALL CHKOUT ('ZZEKQINI'                                      )
         RETURN

      END IF

!
!     Initialize the integer cell, the d.p. array, and the string.
!
      CALL SSIZEI ( ISIZE, EQRYI )
      CALL CLEARD ( DSIZE, EQRYD )
      EQRYC = ' '

!
!     Append enough elements to the integer cell to contain the
!     fixed-size portion of the encoded query:
!
      DO I = 1, EQVBAS
         CALL APPNDI ( 0, EQRYI )
      END DO

!
!     Clear out the fixed-size portion of the integer cell.
!
      CALL CLEARI ( EQVBAS, EQRYI(1) )

!
!     Fill in the architecture version.
!
      EQRYI( EQARCH )  =  1

!
!     Set the parse completion and name and time resolution flags to
!     indicate `not done':
!
      EQRYI(EQPARS)    =  IFALSE
      EQRYI(EQNRES)    =  IFALSE
      EQRYI(EQTRES)    =  IFALSE

!
!     Set the buffer sizes:
!
      EQRYI(EQCSIZ)    =  LEN(EQRYC)
      EQRYI(EQNSIZ)    =  DSIZE

!
!     Set the free pointers:
!
      EQRYI(EQNPTR)    =  1
      EQRYI(EQCPTR)    =  1

!
!     Indicate that initialization has been done:
!
      EQRYI(EQINIT)    =  ITRUE

      CALL CHKOUT ( 'ZZEKQINI' )
      RETURN
      END
