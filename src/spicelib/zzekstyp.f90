!$Procedure      ZZEKSTYP ( EK, determine segment type )

      INTEGER FUNCTION ZZEKSTYP ( NCOLS, CDSCRS )

! $ Abstract
!
!     Determine the type of segment required to support a specified
!     set of columns.
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
!
! $ Declarations

      INCLUDE 'ekcoldsc.inc'
      INCLUDE 'ekglimit.inc'

      INTEGER               NCOLS
      INTEGER               CDSCRS ( CDSCSZ, * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NCOLS      I   Number of columns in the segment.
!     CDSCRS     I   Descriptors of columns.
!
!     The function returns the type of segment that is compatible with
!     the input column descriptors.
!
! $ Detailed_Input
!
!
!     NCOLS          is the number of columns in a new segment.
!
!     CDSCRS         is an array of column descriptors:  the Ith
!                    descriptor applies to the Ith column in the
!                    segment.
!
! $ Detailed_Output
!
!     The function returns the type of segment that is compatible with
!     the input column descriptors.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If NCOLS is non-positive or greater than the maximum allowed
!         number MXCLSG, the error SPICE(INVALIDCOUNT) is signaled.
!
!     2)  If the input column descriptors do not contain compatible
!         attributes, the error SPICE(BADATTRIBUTES) will be signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine determines the appropriate segment type to contain
!     a specified set of columns.  Currently, there are two segment
!     types.  The first type accommodates column classes 1 through 6;
!     the second type accommodates column classes 7 through 9.  The
!     latter set of column classes are `fixed_count' classes:  a column
!     in one of these classes may not have entries added or deleted
!     after the column is created.  Fixed and variable count columns
!     may not coexist in the same segment.
!
! $ Examples
!
!     See EKBSEG.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 06-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               I

      LOGICAL               FIXED
      LOGICAL               VAR

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         ZZEKSTYP = 0
         RETURN
      ELSE
         CALL CHKIN ( 'ZZEKSTYP' )
      END IF

!
!     FIXED and VAR indicate whether we've seen any fixed or variable
!     column classes so far.
!
      FIXED  =  .FALSE.
      VAR    =  .FALSE.

      DO I = 1, NCOLS

         IF (       ( CDSCRS(CLSIDX,I) .GE. 1 ) &
              .AND. ( CDSCRS(CLSIDX,I) .LE. 6 )  ) THEN

            VAR   = .TRUE.

         ELSE IF (       ( CDSCRS(CLSIDX,I) .GE. 7 ) &
                   .AND. ( CDSCRS(CLSIDX,I) .LE. 9 )  ) THEN

            FIXED = .TRUE.

         END IF

      END DO


      IF (  VAR  .AND. ( .NOT. FIXED )  ) THEN

         ZZEKSTYP = 1

      ELSE IF (  FIXED  .AND. ( .NOT. VAR )  ) THEN

         ZZEKSTYP = 2

      ELSE

         ZZEKSTYP = 0

         CALL SETMSG ( 'Column set contains a mixture of variable ' // &
                       'and fixed-count columns.  Segments must '   // &
                       'contain all variable or all fixed count '   // &
                       'columns.'                                    )
         CALL SIGERR ( 'SPICE(BADATTRIBUTES)'                        )
         CALL CHKOUT ( 'ZZEKSTYP'                                    )
         RETURN

      END IF


      CALL CHKOUT ( 'ZZEKSTYP' )
      RETURN
      END
