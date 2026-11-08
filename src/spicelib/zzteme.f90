!$Procedure ZZTEME ( J2000 to TEME at epoch )

      SUBROUTINE ZZTEME( ET, J2TM, TM2J )

! $ Abstract
!
!     J2000 to TEME, and TEME to J2000, probably.
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
!     FRAMES
!
! $ Keywords
!
!     FRAMES
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      J2TM    ( 6, 6 )
      DOUBLE PRECISION      TM2J    ( 6, 6 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch of the state transformation operator.
!     J2TM       O   The state transformation operator J2000 to TEME.
!     TM2J       O   The state transformation operator TEME to J2000.
!
! $ Detailed_Input
!
!     ET         time expressed in terms of TDB seconds past epoch J2000
!                for which to to calculate the J2000 frame to TEME frame
!                transformation operator.
!
! $ Detailed_Output
!
!     J2TM       the 6x6 operator that transforms cartesian states from
!                the J2000 to TEME reference frame.
!
!     TM2J       the 6x6 operator that transforms cartesian states from
!                the TEME to J2000 reference frame.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     TETE    True equator true equinox
!     MEME    Mean equator mean equinox
!     TEME    True equator mean equinox
!
! $ Examples
!
!     None.
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
!     S.C. Krening   (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0, 10-OCT-2021 (EDW)
!
!        Routine now returns TEME to J2000 mapping operator.
!
!-    SPICELIB Version 1.0.0, 06-MAR-2012 (NJB) (SCK) (EDW)
!
!-&

! $ Index_Entries
!
!   transformation J2000 frame to pseudo TEME
!   transformation pseudo TEME to J2000
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      M1     ( 6, 6 )
      DOUBLE PRECISION      M1INV  ( 6, 6 )
      DOUBLE PRECISION      M2     ( 6, 6 )
      DOUBLE PRECISION      M2INV  ( 6, 6 )
      DOUBLE PRECISION      XJ2000 ( 6 )
      DOUBLE PRECISION      Z      ( 6 )
      DOUBLE PRECISION      ZJ2000 ( 6 )

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZTEME' )

!
!     Extract the MEME +X vector and its derivative, both
!     expressed relative to the J2000 frame, into X.
!
      CALL ZZEPRC76( ET, M1    )
      CALL INVSTM  ( M1, M1INV )

      CALL MOVED ( M1INV(1,1), 6, XJ2000 )

!
!     Extract the TETE +Z vector and its derivative, both
!     expressed relative to the MEME frame, into Z.
!
      CALL ZZENUT80( ET, M2    )
      CALL INVSTM  ( M2, M2INV )

      CALL MOVED ( M2INV(1,3), 6, Z )

!
!     Transform Z to the J2000 frame.
!
      CALL MXVG ( M1INV, Z, 6, 6, ZJ2000 )

!
!     Compute the TEME to J2000 state transformation, and the
!     inverse.
!
      CALL ZZTWOVXF ( ZJ2000, 3, XJ2000, 1, TM2J )
      CALL INVSTM   ( TM2J,  J2TM )

      CALL CHKOUT ( 'ZZTEME' )
      RETURN
      END
