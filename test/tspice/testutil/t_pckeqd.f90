!$Procedure T_PCKEQD ( Text PCK equation implementation, 2017 draft )

      SUBROUTINE T_PCKEQD ( BODY,   ET, &
                            RA,     DEC,    PM, &
                            ANGSET, RADSET, RADII )

! $ Abstract
!
!     Umbrella routine for entry points that implement rotational
!     formulas and provide radii data from the current IAU/IAG working
!     group report. These data are meant to be compared against those
!     obtained from the corresponding NAIF text PCK.
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
!     PCK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      PM
      INTEGER               ANGSET ( LBCELL : * )
      INTEGER               RADSET ( LBCELL : * )
      DOUBLE PRECISION      RADII  ( 3 )

! $ Brief_I/O
!
!     Entry Point  Description
!     -----------  --------------------------------------------------
!     T_PCKASTD     Return set of bodies with Euler angle data.
!     T_PCKRSTD     Return set of bodies with radii data.
!     T_PCKANGD     Implement Euler angle equations.
!     T_PCKRADD     Return radii data.
!
! $ Detailed_Input
!
!     See the entry points.
!
! $ Detailed_Output
!
!     See the entry points.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If this routine is called directly, the error
!         SPICE(BOGUSENTRY) is signaled.
!
!     2)  See the headers of the entry points for descriptions of
!         the exceptions specific to those entry points.
!
! $ Files
!
!     The data provided by the entry points of this routine are meant
!     to be consistent with the current IAU report [1] and should be
!     consistent with the NAIF text PCK
!
!        pck00011.tpc
!
!     This routine must be updated along with each NAIF text PCK update.
!
! $ Particulars
!
!     This routine supports testing of the current NAIF text PCK via the
!     entry points
!
!        T_PCKASTD     Return set of bodies with Euler angle data.
!        T_PCKRSTD     Return set of bodies with radii data.
!        T_PCKANGD     Implement Euler angle equations.
!        T_PCKRADD     Return radii data.
!
! $ Examples
!
!     See usage in F_RDPCK.
!
! $ Restrictions
!
!     1)  This routine must be kept consistent with
!         the current NAIF text PCK.
!
!     2)  For use in TSPICE testing, the equations implemented in
!         the entry point T_PCKANGD must be modified to be consistent
!         with those that can be implemented by the SPICE text PCK
!         reader/evaluator algorithms. For example, the M2 nutation
!         precession angle for Mars must be zeroed out, since SPICELIB
!         can't make use of this term.
!
! $ Literature_References
!
!     [1]  Archinal, B.A., Acton, C.H., A'Hearn, M.F., Conrad, A.,
!          Consolmagno, G.J., Duxbury, T., Hestroffer, D., Hilton,
!          J.L., Jorda, L., Kirk, R.L., Klinoner, S.A., McCarthy, D.,
!          Meech, K., Oberst, J., Ping., J., Seidelmann, P.K., Tholen,
!          D.J., Thomas, P.C., and Williams, I.P., "Report of the IAU
!          Working Group on Cartographic Coordinates and Rotational
!          elements: 2015," Celest Mech Dyn Astr (2018) 130:22
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    Test Utilities Version 4.0.0, 16-DEC-2021 (NJB) (JDR)
!
!        Updated to reflect data from the draft 2015 IAU report
!        dated 2017 September 14.
!
!        Edited umbrella routine and all its entry points' headers to
!        comply with NAIF standard.
!
!-    Test Utilities Version 3.0.0, 29-JAN-2011 (NJB)
!
!        Updated to reflect data from the 2009 IAU report.
!
!-    Test Utilities Version 2.0.0, 10-FEB-2010 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD

      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      A0
      DOUBLE PRECISION      D0
      DOUBLE PRECISION      D
      DOUBLE PRECISION      E1
      DOUBLE PRECISION      E2
      DOUBLE PRECISION      E3
      DOUBLE PRECISION      E4
      DOUBLE PRECISION      E5
      DOUBLE PRECISION      E6
      DOUBLE PRECISION      E7
      DOUBLE PRECISION      E8
      DOUBLE PRECISION      E9
      DOUBLE PRECISION      E10
      DOUBLE PRECISION      E11
      DOUBLE PRECISION      E12
      DOUBLE PRECISION      E13
      DOUBLE PRECISION      J1
      DOUBLE PRECISION      J2
      DOUBLE PRECISION      J3
      DOUBLE PRECISION      J4
      DOUBLE PRECISION      J5
      DOUBLE PRECISION      J6
      DOUBLE PRECISION      J7
      DOUBLE PRECISION      J8
      DOUBLE PRECISION      JA
      DOUBLE PRECISION      JB
      DOUBLE PRECISION      JC
      DOUBLE PRECISION      JD
      DOUBLE PRECISION      JE
      DOUBLE PRECISION      M1
      DOUBLE PRECISION      M2
      DOUBLE PRECISION      M3
      DOUBLE PRECISION      M4
      DOUBLE PRECISION      M5
      DOUBLE PRECISION      M6
      DOUBLE PRECISION      M7
      DOUBLE PRECISION      M8
      DOUBLE PRECISION      M9
      DOUBLE PRECISION      M10
      DOUBLE PRECISION      M11
      DOUBLE PRECISION      M12
      DOUBLE PRECISION      M13
      DOUBLE PRECISION      M14
      DOUBLE PRECISION      M15
      DOUBLE PRECISION      M16
      DOUBLE PRECISION      M17
      DOUBLE PRECISION      M18
      DOUBLE PRECISION      M19
      DOUBLE PRECISION      M20
      DOUBLE PRECISION      M21
      DOUBLE PRECISION      M22
      DOUBLE PRECISION      M23
      DOUBLE PRECISION      M24
      DOUBLE PRECISION      M25
      DOUBLE PRECISION      M26
      DOUBLE PRECISION      M27
      DOUBLE PRECISION      M28
      DOUBLE PRECISION      M29
      DOUBLE PRECISION      M30
      DOUBLE PRECISION      M31
      DOUBLE PRECISION      MC1
      DOUBLE PRECISION      MC2
      DOUBLE PRECISION      MC3
      DOUBLE PRECISION      MC4
      DOUBLE PRECISION      MC5
      DOUBLE PRECISION      N
      DOUBLE PRECISION      N1
      DOUBLE PRECISION      N2
      DOUBLE PRECISION      N3
      DOUBLE PRECISION      N4
      DOUBLE PRECISION      N5
      DOUBLE PRECISION      N6
      DOUBLE PRECISION      N7
      DOUBLE PRECISION      S1
      DOUBLE PRECISION      S2
      DOUBLE PRECISION      S3
      DOUBLE PRECISION      S4
      DOUBLE PRECISION      S5
      DOUBLE PRECISION      S6
      DOUBLE PRECISION      T
      DOUBLE PRECISION      U1
      DOUBLE PRECISION      U2
      DOUBLE PRECISION      U3
      DOUBLE PRECISION      U4
      DOUBLE PRECISION      U5
      DOUBLE PRECISION      U6
      DOUBLE PRECISION      U7
      DOUBLE PRECISION      U8
      DOUBLE PRECISION      U9
      DOUBLE PRECISION      U10
      DOUBLE PRECISION      U11
      DOUBLE PRECISION      U12
      DOUBLE PRECISION      U13
      DOUBLE PRECISION      U14
      DOUBLE PRECISION      U15
      DOUBLE PRECISION      U16
      DOUBLE PRECISION      W

      IF ( RETURN() ) THEN
         RETURN
      END IF
!
!     This routine should never be called.
!
      CALL CHKIN ( 'T_PCKEQD' )
      CALL SIGERR( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'T_PCKEQD' )
      RETURN





!$Procedure T_PCKASTD ( Text PCK Euler angle body set )

      ENTRY T_PCKASTD ( ANGSET )

! $ Abstract
!
!     Return the set of NAIF ID codes for those bodies
!     having Euler angles for their orientation.
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
!     PCK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     INTEGER               ANGSET ( LBCELL : * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ANGSET    I-O  Set of ID code for bodies having Euler angle data.
!
! $ Detailed_Input
!
!     ANGSET   is an initialized SPICE set.
!
! $ Detailed_Output
!
!     ANGSET   is a SPICE set containing the ID codes of those
!              bodies for which the current IAU report [1]
!              contains Euler angle data.
!
!              Note that this set does not necessarily contain,
!              nor is it necessarily contained in, the set of
!              bodies for which the current IAU report [1]
!              contains radius data.
!
!              In addition, ANGSET contains the body code -10,
!              which is needed for tests performed by F_RDPCK.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while inserting ID codes into ANGSET, the
!         error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     The data provided by this routine are meant to be consistent with
!     the current IAU report [1] and should be consistent with the NAIF
!     text PCK
!
!        pck00011.tpc
!
!     This routine must be updated along with each NAIF text PCK update.
!
! $ Particulars
!
!     This routine enables comparison of the set of bodies having
!     orientation data in the current IAU report [1] against the
!     corresponding set of bodies in the current NAIF text PCK.
!
!     This comparison serves to minimize the chance of transcription
!     error when a new PCK version is created.
!
! $ Examples
!
!     See use in F_RDPCK.
!
! $ Restrictions
!
!     1)  This routine must be kept consistent with the current IAU
!         report [1].
!
! $ Literature_References
!
!     [1]  Archinal, B.A., Acton, C.H., A'Hearn, M.F., Conrad, A.,
!          Consolmagno, G.J., Duxbury, T., Hestroffer, D., Hilton,
!          J.L., Jorda, L., Kirk, R.L., Klinoner, S.A., McCarthy, D.,
!          Meech, K., Oberst, J., Ping., J., Seidelmann, P.K., Tholen,
!          D.J., Thomas, P.C., and Williams, I.P., "Report of the IAU
!          Working Group on Cartographic Coordinates and Rotational
!          elements: 2015," Celest Mech Dyn Astr (2018) 130:22
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    Test Utilities Version 4.0.0, 30-MAY-2021 (NJB) (JDR)
!
!        Updated to reflect data from the 2015 IAU report.
!
!        Edited the header to comply with NAIF standard.
!
!-    Test Utilities Version 3.0.0, 29-JAN-2011 (NJB)
!
!        Updated to reflect data from the 2009 IAU report.
!
!-    Test Utilities Version 2.0.0, 10-FEB-2010 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


      CALL CHKIN( 'T_PCKASTD' )

      CALL SCARDI( 0,  ANGSET )

!
!     Body -10 is used for testing features
!     not exercised by the normal cases.
!
      CALL INSRTI( -10,         ANGSET )


      CALL INSRTI( 10,          ANGSET )

      CALL INSRTI( 199,         ANGSET )
      CALL INSRTI( 299,         ANGSET )
      CALL INSRTI( 399,         ANGSET )
      CALL INSRTI( 499,         ANGSET )
      CALL INSRTI( 599,         ANGSET )
      CALL INSRTI( 699,         ANGSET )
      CALL INSRTI( 799,         ANGSET )
      CALL INSRTI( 899,         ANGSET )
      CALL INSRTI( 999,         ANGSET )

      CALL INSRTI( 301,         ANGSET )

      CALL INSRTI( 401,         ANGSET )
      CALL INSRTI( 402,         ANGSET )

      CALL INSRTI( 501,         ANGSET )
      CALL INSRTI( 502,         ANGSET )
      CALL INSRTI( 503,         ANGSET )
      CALL INSRTI( 504,         ANGSET )
      CALL INSRTI( 505,         ANGSET )
      CALL INSRTI( 514,         ANGSET )
      CALL INSRTI( 515,         ANGSET )
      CALL INSRTI( 516,         ANGSET )

      CALL INSRTI( 601,         ANGSET )
      CALL INSRTI( 602,         ANGSET )
      CALL INSRTI( 603,         ANGSET )
      CALL INSRTI( 604,         ANGSET )
      CALL INSRTI( 605,         ANGSET )
      CALL INSRTI( 606,         ANGSET )
      CALL INSRTI( 608,         ANGSET )
      CALL INSRTI( 609,         ANGSET )
      CALL INSRTI( 610,         ANGSET )
      CALL INSRTI( 611,         ANGSET )
      CALL INSRTI( 612,         ANGSET )
      CALL INSRTI( 613,         ANGSET )
      CALL INSRTI( 614,         ANGSET )
      CALL INSRTI( 615,         ANGSET )
      CALL INSRTI( 616,         ANGSET )
      CALL INSRTI( 617,         ANGSET )
      CALL INSRTI( 618,         ANGSET )

      CALL INSRTI( 701,         ANGSET )
      CALL INSRTI( 702,         ANGSET )
      CALL INSRTI( 703,         ANGSET )
      CALL INSRTI( 704,         ANGSET )
      CALL INSRTI( 705,         ANGSET )
      CALL INSRTI( 706,         ANGSET )
      CALL INSRTI( 707,         ANGSET )
      CALL INSRTI( 708,         ANGSET )
      CALL INSRTI( 709,         ANGSET )
      CALL INSRTI( 710,         ANGSET )
      CALL INSRTI( 711,         ANGSET )
      CALL INSRTI( 712,         ANGSET )
      CALL INSRTI( 713,         ANGSET )
      CALL INSRTI( 714,         ANGSET )
      CALL INSRTI( 715,         ANGSET )

      CALL INSRTI( 801,         ANGSET )
      CALL INSRTI( 803,         ANGSET )
      CALL INSRTI( 804,         ANGSET )
      CALL INSRTI( 805,         ANGSET )
      CALL INSRTI( 806,         ANGSET )
      CALL INSRTI( 807,         ANGSET )
      CALL INSRTI( 808,         ANGSET )

      CALL INSRTI( 901,         ANGSET )

      CALL INSRTI( 1000005,     ANGSET )
      CALL INSRTI( 1000012,     ANGSET )
      CALL INSRTI( 1000093,     ANGSET )
      CALL INSRTI( 2000001,     ANGSET )
      CALL INSRTI( 2000002,     ANGSET )
      CALL INSRTI( 2000004,     ANGSET )
      CALL INSRTI( 2000021,     ANGSET )
      CALL INSRTI( 2000052,     ANGSET )
      CALL INSRTI( 2000433,     ANGSET )
      CALL INSRTI( 2000511,     ANGSET )
      CALL INSRTI( 2002867,     ANGSET )
      CALL INSRTI( 2025143,     ANGSET )
      CALL INSRTI( 2431010,     ANGSET )
      CALL INSRTI( 9511010,     ANGSET )


!
!     607 is Hyperion.
!
!
!     802 is Nereid.
!
      CALL CHKOUT( 'T_PCKASTD' )
      RETURN




!$Procedure T_PCKRSTD ( Text PCK radii body set )

      ENTRY T_PCKRSTD ( RADSET )

! $ Abstract
!
!     Return the set of NAIF ID codes for those bodies having radii
!     data.
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
!     PCK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     INTEGER               RADSET ( LBCELL : * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     RADSET    I-O  Set of ID code for bodies having radii data.
!
! $ Detailed_Input
!
!     RADSET   is an initialized SPICE set.
!
! $ Detailed_Output
!
!     RADSET   is a SPICE set containing the ID codes of those
!              bodies for which the current IAU report [1]
!              contains radii data.
!
!              Note that this set does not necessarily contain,
!              nor is it necessarily contained in, the set of
!              bodies for which the current IAU report [1]
!              contains radii data.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while inserting ID codes into RADSET, the
!         error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     The data provided by this routine are meant to be consistent with
!     the current IAU report [1] and should be consistent with the NAIF
!     text PCK
!
!        pck00009.tpc
!
!     This routine must be updated along with each NAIF text PCK update.
!
! $ Particulars
!
!     This routine enables comparison of the set of bodies having
!     radii data in the current IAU report [1] against the
!     corresponding set of bodies in the current NAIF text PCK.
!
!     This comparison serves to minimize the chance of transcription
!     error when a new PCK version is created.
!
! $ Examples
!
!     See use in F_RDPCK.
!
! $ Restrictions
!
!     1)  This routine must be kept consistent with the current IAU
!         report [1].
!
! $ Literature_References
!
!     [1]  Archinal, B.A., Acton, C.H., A'Hearn, M.F., Conrad, A.,
!          Consolmagno, G.J., Duxbury, T., Hestroffer, D., Hilton,
!          J.L., Jorda, L., Kirk, R.L., Klinoner, S.A., McCarthy, D.,
!          Meech, K., Oberst, J., Ping., J., Seidelmann, P.K., Tholen,
!          D.J., Thomas, P.C., and Williams, I.P., "Report of the IAU
!          Working Group on Cartographic Coordinates and Rotational
!          elements: 2015," Celest Mech Dyn Astr (2018) 130:22
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    Test Utilities Version 1.0.0, 16-DEC-2021 (NJB) (JDR)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


      CALL CHKIN( 'T_PCKRSTD' )

      CALL SCARDI( 0,  RADSET )

      CALL INSRTI( 10,          RADSET )
      CALL INSRTI( 199,         RADSET )
      CALL INSRTI( 299,         RADSET )
      CALL INSRTI( 399,         RADSET )
      CALL INSRTI( 499,         RADSET )
      CALL INSRTI( 599,         RADSET )
      CALL INSRTI( 699,         RADSET )
      CALL INSRTI( 799,         RADSET )
      CALL INSRTI( 899,         RADSET )
      CALL INSRTI( 999,         RADSET )

      CALL INSRTI( 301,         RADSET )

      CALL INSRTI( 401,         RADSET )
      CALL INSRTI( 402,         RADSET )

      CALL INSRTI( 501,         RADSET )
      CALL INSRTI( 502,         RADSET )
      CALL INSRTI( 503,         RADSET )
      CALL INSRTI( 504,         RADSET )
      CALL INSRTI( 505,         RADSET )
      CALL INSRTI( 506,         RADSET )
      CALL INSRTI( 507,         RADSET )
      CALL INSRTI( 508,         RADSET )
      CALL INSRTI( 509,         RADSET )
      CALL INSRTI( 510,         RADSET )
      CALL INSRTI( 511,         RADSET )
      CALL INSRTI( 512,         RADSET )
      CALL INSRTI( 513,         RADSET )
      CALL INSRTI( 514,         RADSET )
      CALL INSRTI( 515,         RADSET )
      CALL INSRTI( 516,         RADSET )

      CALL INSRTI( 601,         RADSET )
      CALL INSRTI( 602,         RADSET )
      CALL INSRTI( 603,         RADSET )
      CALL INSRTI( 604,         RADSET )
      CALL INSRTI( 605,         RADSET )
      CALL INSRTI( 606,         RADSET )
      CALL INSRTI( 607,         RADSET )
      CALL INSRTI( 608,         RADSET )
      CALL INSRTI( 609,         RADSET )
      CALL INSRTI( 610,         RADSET )
      CALL INSRTI( 611,         RADSET )
      CALL INSRTI( 612,         RADSET )
      CALL INSRTI( 613,         RADSET )
      CALL INSRTI( 614,         RADSET )
      CALL INSRTI( 615,         RADSET )
      CALL INSRTI( 616,         RADSET )
      CALL INSRTI( 617,         RADSET )
      CALL INSRTI( 618,         RADSET )
      CALL INSRTI( 632,         RADSET )
      CALL INSRTI( 633,         RADSET )
      CALL INSRTI( 634,         RADSET )
      CALL INSRTI( 635,         RADSET )
      CALL INSRTI( 649,         RADSET )
      CALL INSRTI( 653,         RADSET )

      CALL INSRTI( 701,         RADSET )
      CALL INSRTI( 702,         RADSET )
      CALL INSRTI( 703,         RADSET )
      CALL INSRTI( 704,         RADSET )
      CALL INSRTI( 705,         RADSET )
      CALL INSRTI( 706,         RADSET )
      CALL INSRTI( 707,         RADSET )
      CALL INSRTI( 708,         RADSET )
      CALL INSRTI( 709,         RADSET )
      CALL INSRTI( 710,         RADSET )
      CALL INSRTI( 711,         RADSET )
      CALL INSRTI( 712,         RADSET )
      CALL INSRTI( 713,         RADSET )
      CALL INSRTI( 714,         RADSET )
      CALL INSRTI( 715,         RADSET )

      CALL INSRTI( 801,         RADSET )
      CALL INSRTI( 802,         RADSET )
      CALL INSRTI( 803,         RADSET )
      CALL INSRTI( 804,         RADSET )
      CALL INSRTI( 805,         RADSET )
      CALL INSRTI( 806,         RADSET )
      CALL INSRTI( 807,         RADSET )
      CALL INSRTI( 808,         RADSET )

      CALL INSRTI( 901,         RADSET )

      CALL INSRTI( 1000005,     RADSET )
      CALL INSRTI( 1000012,     RADSET )
      CALL INSRTI( 1000036,     RADSET )
      CALL INSRTI( 1000041,     RADSET )
      CALL INSRTI( 1000093,     RADSET )
      CALL INSRTI( 1000107,     RADSET )

      CALL INSRTI( 2000001,     RADSET )
      CALL INSRTI( 2000004,     RADSET )
      CALL INSRTI( 2000016,     RADSET )
      CALL INSRTI( 2000021,     RADSET )
      CALL INSRTI( 2000052,     RADSET )
      CALL INSRTI( 2000253,     RADSET )
      CALL INSRTI( 2000433,     RADSET )
      CALL INSRTI( 2000511,     RADSET )
      CALL INSRTI( 2002867,     RADSET )
      CALL INSRTI( 2004179,     RADSET )
      CALL INSRTI( 2025143,     RADSET )
      CALL INSRTI( 2431010,     RADSET )
      CALL INSRTI( 9511010,     RADSET )



      CALL CHKOUT( 'T_PCKRSTD' )
      RETURN








!$Procedure T_PCKANGD ( Text PCK Euler angle equation implementation )

      ENTRY T_PCKANGD ( BODY, ET, RA, DEC, PM )

! $ Abstract
!
!     Generate Euler angles for body orientation directly from
!     in-line coding of equations from the IAU report [1].
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
!     PCK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODY
!     DOUBLE PRECISION      ET
!     DOUBLE PRECISION      RA
!     DOUBLE PRECISION      DEC
!     DOUBLE PRECISION      PM
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   BODY for which Euler angles are to be computed.
!     ET         I   ET for which Euler angles are to be computed.
!     RA         O   RA for BODY at ET.
!     DEC        O   DEC for BODY at ET.
!     PM         O   PM for BODY at ET.
!
! $ Detailed_Input
!
!     BODY     is the NAIF ID code of the body for which Euler
!              angles are to be computed at time ET.
!
!     ET       is the epoch, expressed as seconds past J2000
!              TDB, at which Euler angles giving the
!              orientation of the specified body are to be
!              computed.
!
! $ Detailed_Output
!
!     RA       is the right ascension of the pole of the
!              specified body, relative to the J2000
!              reference frame, at ET.
!
!     DEC      is the declination of the pole of the
!              specified body, relative to the J2000
!              reference frame, at ET.
!
!     PM       is angle of the prime meridian, relative to the
!              ascending node of the body's equator on the
!              J2000 X-Y plane, at ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an unrecognized ID code is provided via the input argument
!         BODY, the error SPICE(INVALIDBODYCODE) is signaled.
!
! $ Files
!
!     The data provided by this routine are meant to be consistent with
!     the current IAU report [1] and should be consistent with the NAIF
!     text PCK
!
!        pck00009.tpc
!
!     This routine must be updated along with each NAIF text PCK update.
!
! $ Particulars
!
!     This routine supports tests of the SPICELIB routines
!
!        BODEUL
!        BODMAT (indirectly, via call to TIPBOD)
!        TIPBOD
!
!     performed by the test family F_RDPCK.
!
!     Results of computations done by this routine are compared with
!     results of computations implemented in the above SPICELIB
!     routines. This testing approach attempts to validate orientation
!     computations done using text PCK data by comparing their results
!     to those obtained via an alternate computational approach.
!
!     These tests also serve to minimize the chance of transcription
!     error when a new PCK version is created. Entering the PCK
!     constants both in the new PCK file and in T_PCKANGD enables
!     double-checking of the constants.
!
! $ Examples
!
!     See use in F_RDPCK.
!
! $ Restrictions
!
!     1)  This routine must be kept consistent with the current IAU
!         report [1].
!
!     2)  For use in TSPICE testing, the equations implemented in the
!         entry point T_PCKANGD must be modified to be consistent with
!         those that can be implemented by the SPICE text PCK
!         reader/evaluator algorithms. For example, the M2 nutation
!         precession angle for Mars must be zeroed out, since SPICELIB
!         can't make use of this term.
!
! $ Literature_References
!
!     [1]  Archinal, B.A., Acton, C.H., A'Hearn, M.F., Conrad, A.,
!          Consolmagno, G.J., Duxbury, T., Hestroffer, D., Hilton,
!          J.L., Jorda, L., Kirk, R.L., Klinoner, S.A., McCarthy, D.,
!          Meech, K., Oberst, J., Ping., J., Seidelmann, P.K., Tholen,
!          D.J., Thomas, P.C., and Williams, I.P., "Report of the IAU
!          Working Group on Cartographic Coordinates and Rotational
!          elements: 2015," Celest Mech Dyn Astr (2018) 130:22
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    Test Utilities Version 4.0.0, 30-MAY-2021 (NJB) (JDR)
!
!        Updated to reflect data from the 2015 IAU report.
!
!        Edited the header to comply with NAIF standard.
!
!-    Test Utilities Version 3.0.0, 29-JAN-2011 (NJB)
!
!        Updated to reflect data from the 2009 IAU report.
!
!-    Test Utilities Version 2.0.0, 10-FEB-2010 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


      CALL CHKIN( 'T_PCKANGD' )


      D  =  ET /   SPD()
      T  =  D  / ( 365.25D0 * 1.D2 )


      IF ( BODY .EQ. 10 ) THEN

         A0  =  286.13D0
         D0  =   63.87D0
         W   =   84.176D0   +   14.1844000D0 * D


      ELSE IF ( BODY .EQ. 199 ) THEN

         MC1 =  ( 174.7910857D0  +   4.092335D0 * D ) * RPD()
         MC2 =  ( 349.5821714D0  +   8.184670D0 * D ) * RPD()
         MC3 =  ( 164.3732571D0  +  12.277005D0 * D ) * RPD()
         MC4 =  ( 339.1643429D0  +  16.369340D0 * D ) * RPD()
         MC5 =  ( 153.9554286D0  +  20.461675D0 * D ) * RPD()


         A0  =  281.0103D0  -  0.0328D0  * T

         D0  =   61.4155D0  -  0.0049D0  * T

         W   =  329.5988D0  +  6.1385108D0  * D &
                            +  0.01067257D0 * SIN( MC1 ) &
                            -  0.00112309D0 * SIN( MC2 ) &
                            -  0.00011040D0 * SIN( MC3 ) &
                            -  0.00002539D0 * SIN( MC4 ) &
                            -  0.00000571D0 * SIN( MC5 )

      ELSE IF ( BODY .EQ. 299 ) THEN

         A0  =   272.76D0
         D0  =    67.16D0
         W   =   160.20D0  -   1.4813688D0  *  D


      ELSE IF ( BODY .EQ. 399 ) THEN

         A0  =     0.00D0  -   0.641D0     * T
         D0  =    90.00D0  -   0.557D0     * T
         W   =   190.147D0 + 360.9856235D0 * D


      ELSE IF ( BODY .EQ. 499 ) THEN

!
!        Phase angles for Mars orientation only.
!
         M16 =  ( 198.991226D0   +        19139.4819985D0 * T ) * RPD()
         M17 =  ( 226.292679D0   +        38280.8511281D0 * T ) * RPD()
         M18 =  ( 249.663391D0   +        57420.7251593D0 * T ) * RPD()
         M19 =  ( 266.183510D0   +        76560.6367950D0 * T ) * RPD()
         M20 =  (  79.398797D0   +            0.5042615D0 * T ) * RPD()

         M21 =  ( 122.433576D0   +        19139.9407476D0 * T ) * RPD()
         M22 =  (  43.058401D0   +        38280.8753272D0 * T ) * RPD()
         M23 =  (  57.663379D0   +        57420.7517205D0 * T ) * RPD()
         M24 =  (  79.476401D0   +        76560.6495004D0 * T ) * RPD()
         M25 =  ( 166.325722D0   +            0.5042615D0 * T ) * RPD()

         M26 =  ( 129.071773D0   +        19140.0328244D0 * T ) * RPD()
         M27 =  (  36.352167D0   +        38281.0473591D0 * T ) * RPD()
         M28 =  (  56.668646D0   +        57420.9295360D0 * T ) * RPD()
         M29 =  (  67.364003D0   +        76560.2552215D0 * T ) * RPD()
         M30 =  ( 104.792680D0   +        95700.4387578D0 * T ) * RPD()
         M31 =  (  95.391654D0   +            0.5042615D0 * T ) * RPD()


         A0  =   317.269202D0   -   0.10927547D0 * T &
                                +   0.000068D0   * SIN( M16 ) &
                                +   0.000238D0   * SIN( M17 ) &
                                +   0.000052D0   * SIN( M18 ) &
                                +   0.000009D0   * SIN( M19 ) &
                                +   0.419057D0   * SIN( M20 )


         D0  =    54.432516D0   -   0.05827105D0 * T &
                                +   0.000051D0   * COS( M21 ) &
                                +   0.000141D0   * COS( M22 ) &
                                +   0.000031D0   * COS( M23 ) &
                                +   0.000005D0   * COS( M24 ) &
                                +   1.591274D0   * COS( M25 )


         W   =   176.049863D0   + 350.891982443297D0 * D &
                                +   0.000145D0   * SIN( M26 ) &
                                +   0.000157D0   * SIN( M27 ) &
                                +   0.000040D0   * SIN( M28 ) &
                                +   0.000001D0   * SIN( M29 ) &
                                +   0.000001D0   * SIN( M30 ) &
                                +   0.584542D0   * SIN( M31 )


      ELSE IF ( BODY .EQ. 599 ) THEN

         JA  = (   99.360714D0  +  4850.4046D0 * T ) * RPD()
         JB  = (  175.895369D0  +  1191.9605D0 * T ) * RPD()
         JC  = (  300.323162D0  +   262.5475D0 * T ) * RPD()
         JD  = (  114.012305D0  +  6070.2476D0 * T ) * RPD()
         JE  = (   49.511251D0  +    64.3000D0 * T ) * RPD()


         A0  =   268.056595D0  -  0.006499D0  * T &
                               +  0.000117D0  * SIN( JA ) &
                               +  0.000938D0  * SIN( JB ) &
                               +  0.001432D0  * SIN( JC ) &
                               +  0.000030D0  * SIN( JD ) &
                               +  0.002150D0  * SIN( JE )

         D0  =    64.495303D0  +  0.002413D0  * T &
                               +  0.000050D0  * COS( JA ) &
                               +  0.000404D0  * COS( JB ) &
                               +  0.000617D0  * COS( JC ) &
                               -  0.000013D0  * COS( JD ) &
                               +  0.000926D0  * COS( JE )

         W   =  284.95D0   +  870.5360000D0 * D


      ELSE IF ( BODY .EQ. 699 ) THEN

         A0  =    40.589D0   -   0.036D0     * T
         D0  =    83.537D0   -   0.004D0     * T
         W   =    38.90D0    + 810.7939024D0 * D


      ELSE IF ( BODY .EQ. 799 ) THEN

         A0  =   257.311D0
         D0  =   -15.175D0
         W   =   203.81D0  -  501.1600928D0 * D


      ELSE IF ( BODY .EQ. 899 ) THEN

         N   =  ( 357.85D0  +  52.316D0 * T ) * RPD()

         A0  =    299.36D0  +   0.70D0  * SIN( N )

         D0  =     43.46D0  -   0.51D0  * COS( N )

         W   =    285.946D0  + 541.1397757D0 * D &
                            -   0.48D0      * SIN( N )


      ELSE IF ( BODY .EQ. 999 ) THEN

         A0  =    132.993D0
         D0  =     -6.163D0
         W   =    302.695D0  + 56.3625225D0 * D


      ELSE IF (  BODY .EQ. 301 ) THEN

         E1  =  ( 125.045D0   -   0.0529921D0  * D ) * RPD()
         E2  =  ( 250.089D0   -   0.1059842D0  * D ) * RPD()
         E3  =  ( 260.008D0   +  13.0120009D0  * D ) * RPD()
         E4  =  ( 176.625D0   +  13.3407154D0  * D ) * RPD()
         E5  =  ( 357.529D0   +   0.9856003D0  * D ) * RPD()
         E6  =  ( 311.589D0   +  26.4057084D0  * D ) * RPD()
         E7  =  ( 134.963D0   +  13.0649930D0  * D ) * RPD()
         E8  =  ( 276.617D0   +   0.3287146D0  * D ) * RPD()
         E9  =  (  34.226D0   +   1.7484877D0  * D ) * RPD()
         E10 =  (  15.134D0   -   0.1589763D0  * D ) * RPD()
         E11 =  ( 119.743D0   +   0.0036096D0  * D ) * RPD()
         E12 =  ( 239.961D0   +   0.1643573D0  * D ) * RPD()
         E13 =  (  25.053D0   +  12.9590088D0  * D ) * RPD()


         A0  =   269.9949D0   +   0.0031D0 * T &
                              -   3.8787D0 * SIN( E1 ) &
                              -   0.1204D0 * SIN( E2 ) &
                              +   0.0700D0 * SIN( E3 ) &
                              -   0.0172D0 * SIN( E4 ) &
                              +   0.0072D0 * SIN( E6 ) &
                              -   0.0052D0 * SIN( E10 ) &
                              +   0.0043D0 * SIN( E13 )


         D0  =    66.5392D0   +   0.0130D0 * T &
                              +   1.5419D0 * COS( E1 ) &
                              +   0.0239D0 * COS( E2 ) &
                              -   0.0278D0 * COS( E3 ) &
                              +   0.0068D0 * COS( E4 ) &
                              -   0.0029D0 * COS( E6 ) &
                              +   0.0009D0 * COS( E7 ) &
                              +   0.0008D0 * COS( E10 ) &
                              -   0.0009D0 * COS( E13 )


         W   =    38.3213D0   +  13.17635815D0 * D &
                              -   1.4D-12      * D * D &
                              +   3.5610D0     * SIN( E1 ) &
                              +   0.1208D0     * SIN( E2 ) &
                              -   0.0642D0     * SIN( E3 ) &
                              +   0.0158D0     * SIN( E4 ) &
                              +   0.0252D0     * SIN( E5 ) &
                              -   0.0066D0     * SIN( E6 ) &
                              -   0.0047D0     * SIN( E7 ) &
                              -   0.0046D0     * SIN( E8 ) &
                              +   0.0028D0     * SIN( E9 ) &
                              +   0.0052D0     * SIN( E10 ) &
                              +   0.0040D0     * SIN( E11 ) &
                              +   0.0019D0     * SIN( E12 ) &
                              -   0.0044D0     * SIN( E13 )


      ELSE IF ( BODY/100 .EQ. 4 ) THEN

!
!        For each of M5, M6, and M7, the quadratic term is omitted from
!        the coded equation but is shown in comments. The kernel pool
!        software cannot handle the quadratic term; trying both of
!        these expressions allows the user to observe the effect of
!        ignoring that term.
!
         M1  =  ( 190.72646643D0   +     15917.10818695D0 * T ) * RPD()
         M2  =  (  21.46892470D0   +     31834.27934054D0 * T ) * RPD()
         M3  =  ( 332.86082793D0   +     19139.89694742D0 * T ) * RPD()
         M4  =  ( 394.93256437D0   +     38280.79631835D0 * T ) * RPD()
         M5  =  (          189.63271560D0 &
                  +   41215158.18420050D0 * T &
                  +         12.71192322D0 * T**2 ) * RPD()
!
!           M5 quadratic term: 12.71192322 * T**2
!
         M6  =  (           19.26538605D0 &
                  +   82430316.36864280D0 * T &
                  +         25.42412173D0 * T**2 ) * RPD()
!
!           M6 quadratic term: 25.42412173 * T**2
!
         M7  =  (         208.89882434D0 &
                  + 123645474.54466790D0 * T &
                  +        38.13293168D0 * T**2 ) * RPD()
!
!           M7 quadratic term: 38.13293168 * T**2
!

         M8  =  ( 121.46893664D0   +       660.22803474D0 * T ) * RPD()
         M9  =  ( 231.05028581D0   +       660.99123540D0 * T ) * RPD()
         M10 =  ( 251.37314025D0   +      1320.50145245D0 * T ) * RPD()
         M11 =  ( 217.98635955D0   +     38279.96125550D0 * T ) * RPD()
         M12 =  ( 196.19729402D0   +     19139.83628608D0 * T ) * RPD()
         M13 =  (  10.80071482D0   +  10414879.22849759D0 * T ) * RPD()
         M14 =  ( 345.99306351D0   +   4801583.39793913D0 * T ) * RPD()
         M15 =  ( 303.28024985D0   +  10415546.40050500D0 * T ) * RPD()

!
!        See the formulas for Mars for constants defining
!        additional phase angles numbered 16-31.
!


         IF ( BODY .EQ. 401 ) THEN


            A0  =   317.67071657D0  -    0.10844326D0 * T &
                                    -    1.78428399D0 * SIN( M1 ) &
                                    +    0.02212824D0 * SIN( M2 ) &
                                    -    0.01028251D0 * SIN( M3 ) &
                                    -    0.00475595D0 * SIN( M4 )

            D0  =    52.88627266D0  -    0.06134706D0 * T &
                                    -    1.07516537D0 * COS( M1 ) &
                                    +    0.00668626D0 * COS( M2 ) &
                                    -    0.00648740D0 * COS( M3 ) &
                                    +    0.00281576D0 * COS( M4 )

            W   =    35.18774440D0  + 1128.84475928D0 * D &
                                    +   12.72192797D0 * T * T &
                                    +    1.42421769D0 * SIN( M1 ) &
                                    -    0.02273783D0 * SIN( M2 ) &
                                    +    0.00410711D0 * SIN( M3 ) &
                                    +    0.00631964D0 * SIN( M4 ) &
                                    +    1.73203319D0 * SIN( M5 ) &
                                    +    0.01635407D0 * SIN( M6 ) &
                                    +    0.00021426D0 * SIN( M7 )


         ELSE IF ( BODY .EQ. 402 ) THEN


            A0  =   316.65705808D0  -  0.10518014D0  * T &
                                    +  3.09217726D0  * SIN( M8  ) &
                                    +  0.22980637D0  * SIN( M9  ) &
                                    +  0.06418655D0  * SIN( M10 ) &
                                    +  0.02533537D0  * SIN( M11 ) &
                                    +  0.00778695D0  * SIN( M12 )



            D0  =    53.50992033D0  -   0.05979094D0  * T &
                                    +   1.83936004D0  * COS( M8  ) &
                                    +   0.14325320D0  * COS( M9  ) &
                                    +   0.01911409D0  * COS( M10 ) &
                                    -   0.01482590D0  * COS( M11 ) &
                                    +   0.00192430D0  * COS( M12 )



            W   =    79.39932954D0  + 285.16188899D0 * D &
                                    -   2.73954829D0 * SIN( M8  ) &
                                    -   0.39968606D0 * SIN( M9  ) &
                                    -   0.06563259D0 * SIN( M10 ) &
                                    -   0.02912940D0 * SIN( M11 ) &
                                    +   0.01699160D0 * SIN( M12 ) &
                                    +   0.03080596D0 * SIN( M13 ) &
                                    +   0.01248044D0 * SIN( M14 ) &
                                    -   0.00437509D0 * SIN( M15 )


         ELSE

            CALL SETMSG ( 'Body code # was not recognized.' )
            CALL ERRINT ( '#', BODY                         )
            CALL SIGERR ( 'SPICE(INVALIDBODYCODE)'          )
            CALL CHKOUT ( 'T_PCKANGD'                        )
            RETURN

         END IF




      ELSE IF ( BODY/100 .EQ. 5 ) THEN


         J1  =  (    73.32D0  +  91472.9D0 * T ) * RPD()
         J2  =  (    24.62D0  +  45137.2D0 * T ) * RPD()
         J3  =  (   283.90D0  +   4850.7D0 * T ) * RPD()
         J4  =  (   355.80D0  +   1191.3D0 * T ) * RPD()
         J5  =  (   119.90D0  +    262.1D0 * T ) * RPD()
         J6  =  (   229.80D0  +     64.3D0 * T ) * RPD()
         J7  =  (   352.25D0  +   2382.6D0 * T ) * RPD()
         J8  =  (   113.35D0  +   6070.0D0 * T ) * RPD()


         IF ( BODY .EQ. 501 ) THEN

            A0  =    268.05D0    -   0.009D0 * T &
                                 +   0.094D0 * SIN( J3 ) &
                                 +   0.024D0 * SIN( J4 )

            D0  =     64.50D0    +   0.003D0 * T &
                                 +   0.040D0 * COS( J3 ) &
                                 +   0.011D0 * COS( J4 )

            W   =    200.39D0    + 203.4889538D0 * D &
                                 -   0.085D0     * SIN( J3 ) &
                                 -   0.022D0     * SIN( J4 )


         ELSE IF ( BODY .EQ. 502 ) THEN

            A0  =    268.08D0    -   0.009D0 * T &
                                 +   1.086D0 * SIN( J4 ) &
                                 +   0.060D0 * SIN( J5 ) &
                                 +   0.015D0 * SIN( J6 ) &
                                 +   0.009D0 * SIN( J7 )

            D0  =     64.51D0    +   0.003D0 * T &
                                 +   0.468D0 * COS( J4 ) &
                                 +   0.026D0 * COS( J5 ) &
                                 +   0.007D0 * COS( J6 ) &
                                 +   0.002D0 * COS( J7 )

            W   =     36.022D0   + 101.3747235D0 * D &
                                 -   0.980D0     * SIN( J4 ) &
                                 -   0.054D0     * SIN( J5 ) &
                                 -   0.014D0     * SIN( J6 ) &
                                 -   0.008D0     * SIN( J7 )


         ELSE IF ( BODY .EQ. 503 ) THEN

            A0  =    268.20D0    -   0.009D0 * T &
                                 -   0.037D0 * SIN( J4 ) &
                                 +   0.431D0 * SIN( J5 ) &
                                 +   0.091D0 * SIN( J6 )

            D0  =     64.57D0    +   0.003D0 * T &
                                 -   0.016D0 * COS( J4 ) &
                                 +   0.186D0 * COS( J5 ) &
                                 +   0.039D0 * COS( J6 )


            W   =     44.064D0   +  50.3176081D0 * D &
                                 +   0.033D0     * SIN( J4 ) &
                                 -   0.389D0     * SIN( J5 ) &
                                 -   0.082D0     * SIN( J6 )



         ELSE IF ( BODY .EQ. 504 ) THEN

            A0  =    268.72D0    -   0.009D0 * T &
                                 -   0.068D0 * SIN( J5 ) &
                                 +   0.590D0 * SIN( J6 ) &
                                 +   0.010D0 * SIN( J8 )

            D0  =     64.83D0    +   0.003D0 * T &
                                 -   0.029D0 * COS( J5 ) &
                                 +   0.254D0 * COS( J6 ) &
                                 -   0.004D0 * COS( J8 )

            W   =    259.51D0    +  21.5710715D0 * D &
                                 +   0.061D0     * SIN( J5 ) &
                                 -   0.533D0     * SIN( J6 ) &
                                 -   0.009D0     * SIN( J8 )



         ELSE IF ( BODY .EQ. 505 ) THEN


            A0  =    268.05D0    -   0.009D0 * T &
                                 -   0.84D0  * SIN(     J1 ) &
                                 +   0.01D0  * SIN( 2 * J1 )

            D0  =     64.49D0    +   0.003D0 * T &
                                 -   0.36D0  * COS( J1 )

            W   =    231.67D0    + 722.6314560D0 * D &
                                 +   0.76D0      * SIN(     J1 ) &
                                 -   0.01D0      * SIN( 2 * J1 )


         ELSE IF ( BODY .EQ. 514 ) THEN

            A0  =    268.05D0    -   0.009D0 * T &
                                 -   2.11D0  * SIN(     J2 ) &
                                 +   0.04D0  * SIN( 2 * J2 )

            D0  =     64.49D0    +   0.003D0 * T &
                                 -   0.91D0  * COS(     J2 ) &
                                 +   0.01D0  * COS( 2 * J2 )

            W   =      8.56D0    + 533.7004100D0 * D &
                                 +   1.91D0      * SIN(     J2 ) &
                                 -   0.04D0      * SIN( 2 * J2 )


         ELSE IF ( BODY .EQ. 515 ) THEN


            A0  =   268.05D0     -    0.009D0     * T
            D0  =   64.49D0      +    0.003D0     * T
            W   =   33.29D0      + 1206.9986602D0 * D



         ELSE IF ( BODY .EQ. 516 ) THEN

            A0  =   268.05D0     -    0.009D0     * T
            D0  =    64.49D0     +    0.003D0     * T
            W   =   346.09D0     + 1221.2547301D0 * D


         ELSE

            CALL SETMSG ( 'Body code # was not recognized.' )
            CALL ERRINT ( '#', BODY                         )
            CALL SIGERR ( 'SPICE(INVALIDBODYCODE)'          )
            CALL CHKOUT ( 'T_PCKANGD'                        )
            RETURN


         END IF



      ELSE IF ( BODY/100 .EQ. 6 ) THEN

         S1  =  (  353.32D0   +   75706.7D0 * T ) * RPD()
         S2  =  (   28.72D0   +   75706.7D0 * T ) * RPD()
         S3  =  (  177.40D0   -   36505.5D0 * T ) * RPD()
         S4  =  (  300.00D0   -    7225.9D0 * T ) * RPD()
         S5  =  (  316.45D0   +     506.2D0 * T ) * RPD()
         S6  =  (  345.20D0   -    1016.3D0 * T ) * RPD()


         IF ( BODY .EQ. 601 ) THEN

            A0  =   40.66D0   -       0.036D0 * T &
                              +      13.56D0  * SIN( S3 )

            D0  =   83.52D0   -       0.004D0 * T &
                              -       1.53D0  * COS( S3 )

            W   =  333.46D0   +     381.9945550D0 * D &
                              -      13.48D0      * SIN( S3 ) &
                              -      44.85D0      * SIN( S5 )



         ELSE IF ( BODY .EQ. 602 ) THEN

            A0  =   40.66D0   -       0.036D0 * T
            D0  =   83.52D0   -       0.004D0 * T
            W   =    6.32D0   +     262.7318996D0 * D



         ELSE IF ( BODY .EQ. 603 ) THEN

            A0  =   40.66D0   -       0.036D0 * T &
                              +       9.66D0  * SIN( S4 )

            D0  =   83.52D0   -       0.004D0 * T &
                              -       1.09D0  * COS( S4 )

            W   =    8.95D0   +     190.6979085D0 * D &
                              -       9.60D0  * SIN( S4 ) &
                              +       2.23D0  * SIN( S5 )


         ELSE IF ( BODY .EQ. 604 ) THEN


           A0  =    40.66D0   -       0.036D0 * T

           D0  =    83.52D0   -       0.004D0 * T

           W   =   357.6D0    +     131.5349316D0 * D



         ELSE IF ( BODY .EQ. 605 ) THEN


            A0  =   40.38D0   -       0.036D0 * T &
                              +       3.10D0  * SIN( S6 )

            D0  =   83.55D0   -       0.004D0 * T &
                              -       0.35D0  * COS( S6 )

            W   =  235.16D0   +      79.6900478D0 * D &
                              -       3.08D0  * SIN( S6 )

         ELSE IF ( BODY .EQ. 606 ) THEN


            A0  =   39.4827D0

            D0  =   83.4279D0

            W   =  186.5855D0 + 22.5769768D0 * D


         ELSE IF ( BODY .EQ. 608 ) THEN

            A0  =  318.16D0   -      3.949D0 * T
            D0  =   75.03D0   -      1.143D0 * T
            W   =  355.2D0    +      4.5379572D0 * D


         ELSE IF ( BODY .EQ. 609 ) THEN

            A0  =  356.90D0
            D0  =   77.80D0
            W   =  178.58D0   +    931.639D0 * D


         ELSE IF ( BODY .EQ. 610 ) THEN

            A0  =   40.58D0   -     0.036D0 * T &
                              -     1.623D0 * SIN(     S2 ) &
                              +     0.023D0 * SIN( 2 * S2 )

            D0  =   83.52D0   -     0.004D0 * T &
                              -     0.183D0 * COS(     S2 ) &
                              +     0.001D0 * COS( 2 * S2 )

            W   =   58.83D0   +   518.2359876D0 * D &
                              +     1.613D0 * SIN(     S2 ) &
                              -     0.023D0 * SIN( 2 * S2 )


         ELSE IF ( BODY .EQ. 611 ) THEN

            A0  =   40.58D0   -     0.036D0 * T &
                              -     3.153D0 * SIN(     S1 ) &
                              +     0.086D0 * SIN( 2 * S1 )

            D0  =   83.52D0   -     0.004D0 * T &
                              -     0.356D0 * COS(     S1 ) &
                              +     0.005D0 * COS( 2 * S1 )

            W   =  293.87D0   +   518.4907239D0 * D &
                              +     3.133D0 * SIN(     S1 ) &
                              -     0.086D0 * SIN( 2 * S1 )



         ELSE IF ( BODY .EQ. 612 ) THEN

            A0  =  40.85D0  -    0.036D0 * T
            D0  =  83.34D0  -    0.004D0 * T
            W   = 245.12D0  +  131.6174056D0 * D


         ELSE IF ( BODY .EQ. 613 ) THEN

            A0  =  50.51D0  -    0.036D0 * T
            D0  =  84.06D0  -    0.004D0 * T
            W   =  56.88D0  +  190.6979332D0 * D


         ELSE IF ( BODY .EQ. 614 ) THEN

            A0  =  36.41D0  -    0.036D0 * T

            D0  =  85.04D0  -    0.004D0 * T

            W   = 153.51D0  + 190.6742373D0 * D


         ELSE IF ( BODY .EQ. 615 ) THEN

            A0  =  40.58D0  -   0.036D0 * T
            D0  =  83.53D0  -   0.004D0 * T
            W   = 137.88D0  + 598.3060000D0 * D


         ELSE IF ( BODY .EQ. 616 ) THEN

            A0  =  40.58D0  -   0.036D0 * T
            D0  =  83.53D0  -   0.004D0 * T
            W   = 296.14D0  + 587.289000D0 * D


         ELSE IF ( BODY .EQ. 617 ) THEN

            A0  =  40.58D0  -   0.036D0 * T
            D0  =  83.53D0  -   0.004D0 * T
            W   = 162.92D0  + 572.7891000D0 * D


         ELSE IF ( BODY .EQ. 618 ) THEN

            A0  =  40.6D0   -   0.036D0 * T
            D0  =  83.5D0   -   0.004D0 * T
            W   =  48.8D0   + 626.0440000D0 * D

         ELSE

            CALL SETMSG ( 'Body code # was not recognized.' )
            CALL ERRINT ( '#', BODY                         )
            CALL SIGERR ( 'SPICE(INVALIDBODYCODE)'          )
            CALL CHKOUT ( 'T_PCKANGD'                        )
            RETURN

         END IF



      ELSE IF ( BODY/100 .EQ. 7 ) THEN


         U1   =  (  115.75D0  +   54991.87D0 * T ) * RPD()
         U2   =  (  141.69D0  +   41887.66D0 * T ) * RPD()
         U3   =  (  135.03D0  +   29927.35D0 * T ) * RPD()
         U4   =  (   61.77D0  +   25733.59D0 * T ) * RPD()
         U5   =  (  249.32D0  +   24471.46D0 * T ) * RPD()
         U6   =  (   43.86D0  +   22278.41D0 * T ) * RPD()
         U7   =  (   77.66D0  +   20289.42D0 * T ) * RPD()
         U8   =  (  157.36D0  +   16652.76D0 * T ) * RPD()
         U9   =  (  101.81D0  +   12872.63D0 * T ) * RPD()
         U10  =  (  138.64D0  +    8061.81D0 * T ) * RPD()
         U11  =  (  102.23D0  -    2024.22D0 * T ) * RPD()
         U12  =  (  316.41D0  +    2863.96D0 * T ) * RPD()
         U13  =  (  304.01D0  -      51.94D0 * T ) * RPD()
         U14  =  (  308.71D0  -      93.17D0 * T ) * RPD()
         U15  =  (  340.82D0  -      75.32D0 * T ) * RPD()
         U16  =  (  259.14D0  -     504.81D0 * T ) * RPD()



         IF ( BODY .EQ. 701 ) THEN


            A0  =   257.43D0   +   0.29D0 * SIN( U13 )

            D0  =   -15.10D0   +   0.28D0 * COS( U13 )

            W   =   156.22D0   - 142.8356681D0 * D &
                               +   0.05D0    * SIN( U12 ) &
                               +   0.08D0    * SIN( U13 )


         ELSE IF ( BODY .EQ. 702 ) THEN


            A0  =   257.43D0   +   0.21D0 * SIN( U14 )

            D0  =   -15.10D0   +   0.20D0 * COS( U14 )

            W   =   108.05D0   -  86.8688923D0 * D &
                               -   0.09D0 * SIN( U12 ) &
                               +   0.06D0 * SIN( U14 )


         ELSE IF ( BODY .EQ. 703 ) THEN


            A0  =   257.43D0   +   0.29D0 * SIN( U15 )

            D0  =   -15.10D0   +   0.28D0 * COS( U15 )

            W   =    77.74D0   -  41.3514316D0 * D &
                               +   0.08D0 * SIN( U15 )



         ELSE IF ( BODY .EQ. 704 ) THEN

            A0  =   257.43D0   +   0.16D0 * SIN( U16 )

            D0  =   -15.10D0   +   0.16D0 * COS( U16 )

            W   =     6.77D0   -  26.7394932D0 * D &
                               +   0.04D0 * SIN( U16 )


         ELSE IF ( BODY .EQ. 705 ) THEN

            A0  =   257.43D0   +   4.41D0 * SIN(     U11 ) &
                               -   0.04D0 * SIN( 2 * U11 )

            D0  =   -15.08D0   +   4.25D0 * COS(     U11 ) &
                               -   0.02D0 * COS( 2 * U11 )

            W   =    30.70D0   - 254.6906892D0 * D &
                               -   1.27D0 * SIN(     U12 ) &
                               +   0.15D0 * SIN( 2 * U12 ) &
                               +   1.15D0 * SIN(     U11 ) &
                               -   0.09D0 * SIN( 2 * U11 )


         ELSE IF ( BODY .EQ. 706 ) THEN

            A0  =   257.31D0   -    0.15D0 * SIN( U1 )

            D0  =   -15.18D0   +    0.14D0 * COS( U1 )

            W   =   127.69D0   - 1074.5205730D0 * D &
                               -    0.04D0 * SIN( U1 )


         ELSE IF ( BODY .EQ. 707 ) THEN

            A0  =   257.31D0   -    0.09D0 * SIN( U2 )

            D0  =   -15.18D0   +    0.09D0 * COS( U2 )

            W   =   130.35D0   -  956.4068150D0 * D &
                               -    0.03D0 * SIN( U2 )



         ELSE IF ( BODY .EQ. 708 ) THEN

            A0  =   257.31D0   -    0.16D0 * SIN( U3 )

            D0  =   -15.18D0   +    0.16D0 * COS( U3 )

            W   =   105.46D0   -  828.3914760D0  * D &
                               -    0.04D0 * SIN( U3 )



         ELSE IF ( BODY .EQ. 709 ) THEN

            A0  =   257.31D0   -    0.04D0 * SIN( U4 )

            D0  =   -15.18D0   +    0.04D0 * COS( U4 )

            W   =    59.16D0   -  776.5816320D0 * D &
                               -    0.01D0 * SIN( U4 )



         ELSE IF ( BODY .EQ. 710 ) THEN

            A0  =   257.31D0   -    0.17D0 * SIN( U5 )

            D0  =   -15.18D0   +    0.16D0 * COS( U5 )

            W   =    95.08D0   -  760.0531690D0 * D &
                               -    0.04D0 * SIN( U5 )



         ELSE IF ( BODY .EQ. 711 ) THEN

            A0  =   257.31D0   -    0.06D0 * SIN( U6 )

            D0  =   -15.18D0   +    0.06D0 * COS( U6 )

            W   =   302.56D0   -  730.1253660D0 * D &
                               -    0.02D0 * SIN( U6 )



         ELSE IF ( BODY .EQ. 712 ) THEN

            A0  =   257.31D0   -    0.09D0 * SIN( U7 )

            D0  =   -15.18D0   +    0.09D0 * COS( U7 )

            W   =    25.03D0   -  701.4865870D0 * D &
                               -    0.02D0 * SIN( U7 )



         ELSE IF ( BODY .EQ. 713 ) THEN

            A0  =   257.31D0   -    0.29D0 * SIN( U8 )

            D0  =   -15.18D0   +    0.28D0 * COS( U8 )

            W   =   314.90D0   -  644.6311260D0 * D &
                               -    0.08D0 * SIN( U8 )



         ELSE IF ( BODY .EQ. 714 ) THEN

            A0  =   257.31D0   -    0.03D0 * SIN( U9 )

            D0  =   -15.18D0   +    0.03D0 * COS( U9 )

            W   =   297.46D0   -  577.3628170D0 * D &
                               -    0.01D0 * SIN( U9 )


         ELSE IF ( BODY .EQ. 715 ) THEN

            A0  =   257.31D0   -    0.33D0 * SIN( U10 )

            D0  =   -15.18D0   +    0.31D0 * COS( U10 )

            W   =    91.24D0   -  472.5450690D0 * D &
                               -    0.09D0 * SIN( U10 )


         ELSE

            CALL SETMSG ( 'Body code # was not recognized.' )
            CALL ERRINT ( '#', BODY                         )
            CALL SIGERR ( 'SPICE(INVALIDBODYCODE)'          )
            CALL CHKOUT ( 'T_PCKANGD'                        )
            RETURN

         END IF



      ELSE IF ( BODY/100 .EQ. 8 ) THEN


         N     = (     357.85D0 +     52.316D0 * T ) * RPD()
         N1    = (     323.92D0 +  62606.6D0   * T ) * RPD()
         N2    = (     220.51D0 +  55064.2D0   * T ) * RPD()
         N3    = (     354.27D0 +  46564.5D0   * T ) * RPD()
         N4    = (      75.31D0 +  26109.4D0   * T ) * RPD()
         N5    = (      35.36D0 +  14325.4D0   * T ) * RPD()
         N6    = (     142.61D0 +   2824.6D0   * T ) * RPD()
         N7    = (     177.85D0 +     52.316D0 * T ) * RPD()


         IF ( BODY .EQ. 801 ) THEN

            A0  =   299.36D0   -   32.35D0 * SIN(     N7 ) &
                               -    6.28D0 * SIN( 2 * N7 ) &
                               -    2.08D0 * SIN( 3 * N7 ) &
                               -    0.74D0 * SIN( 4 * N7 ) &
                               -    0.28D0 * SIN( 5 * N7 ) &
                               -    0.11D0 * SIN( 6 * N7 ) &
                               -    0.07D0 * SIN( 7 * N7 ) &
                               -    0.02D0 * SIN( 8 * N7 ) &
                               -    0.01D0 * SIN( 9 * N7 )

            D0  =    41.17D0   +   22.55D0 * COS(     N7 ) &
                               +    2.10D0 * COS( 2 * N7 ) &
                               +    0.55D0 * COS( 3 * N7 ) &
                               +    0.16D0 * COS( 4 * N7 ) &
                               +    0.05D0 * COS( 5 * N7 ) &
                               +    0.02D0 * COS( 6 * N7 ) &
                               +    0.01D0 * COS( 7 * N7 )


            W   =   296.53D0   -   61.2572637D0 * D &
                               +   22.25D0 * SIN(     N7 ) &
                               +    6.73D0 * SIN( 2 * N7 ) &
                               +    2.05D0 * SIN( 3 * N7 ) &
                               +    0.74D0 * SIN( 4 * N7 ) &
                               +    0.28D0 * SIN( 5 * N7 ) &
                               +    0.11D0 * SIN( 6 * N7 ) &
                               +    0.05D0 * SIN( 7 * N7 ) &
                               +    0.02D0 * SIN( 8 * N7 ) &
                               +    0.01D0 * SIN( 9 * N7 )


         ELSE IF ( BODY .EQ. 803 ) THEN

            A0  =   299.36D0   +    0.70D0 * SIN(     N  ) &
                               -    6.49D0 * SIN(     N1 ) &
                               +    0.25D0 * SIN( 2 * N1 )

            D0  =    43.36D0   -    0.51D0 * COS(     N  ) &
                               -    4.75D0 * COS(     N1 ) &
                               +    0.09D0 * COS( 2 * N1 )

            W   =   254.06D0   + 1222.8441209D0 * D &
                               -    0.48D0 * SIN(     N  ) &
                               +    4.40D0 * SIN(     N1 ) &
                               -    0.27D0 * SIN( 2 * N1 )



         ELSE IF ( BODY .EQ. 804 ) THEN

            A0  =   299.36D0   +    0.70D0 * SIN(     N  ) &
                               -    0.28D0 * SIN(     N2 )

            D0  =    43.45D0   -    0.51D0 * COS(     N  ) &
                               -    0.21D0 * COS(     N2 )

            W   =   102.06D0   + 1155.7555612D0 * D &
                               -    0.48D0 * SIN(     N  ) &
                               +    0.19D0 * SIN(     N2 )


         ELSE IF ( BODY .EQ. 805 ) THEN

            A0  =   299.36D0   +    0.70D0 * SIN(     N  ) &
                               -    0.09D0 * SIN(     N3 )

            D0  =    43.45D0   -    0.51D0 * COS(     N  ) &
                               -    0.07D0 * COS(     N3 )

            W   =   306.51D0   + 1075.7341562D0 * D &
                               -    0.49D0 * SIN(     N  ) &
                               +    0.06D0 * SIN(     N3 )


         ELSE IF ( BODY .EQ. 806 ) THEN

            A0  =   299.36D0   +    0.70D0 * SIN(     N  ) &
                               -    0.07D0 * SIN(     N4 )

            D0  =    43.43D0   -    0.51D0 * COS(     N  ) &
                               -    0.05D0 * COS(     N4 )

            W   =   258.09D0   +  839.6597686D0 * D &
                               -    0.48D0 * SIN(     N  ) &
                               +    0.05D0 * SIN(     N4 )


         ELSE IF ( BODY .EQ. 807 ) THEN

            A0  =   299.36D0   +    0.70D0 * SIN(     N  ) &
                               -    0.27D0 * SIN(     N5 )

            D0  =    43.41D0   -    0.51D0 * COS(     N  ) &
                               -    0.20D0 * COS(     N5 )

            W   =   179.41D0   +  649.0534470D0 * D &
                               -    0.48D0 * SIN(     N  ) &
                               +    0.19D0 * SIN(     N5 )



         ELSE IF ( BODY .EQ. 808 ) THEN

            A0  =   299.27D0   +    0.70D0 * SIN(     N  ) &
                               -    0.05D0 * SIN(     N6 )

            D0  =    42.91D0   -    0.51D0 * COS(     N  ) &
                               -    0.04D0 * COS(     N6 )

            W   =    93.38D0   +  320.7654228D0 * D &
                               -    0.48D0 * SIN(     N  ) &
                               +    0.04D0 * SIN(     N6 )




         ELSE

            CALL SETMSG ( 'Body code # was not recognized.' )
            CALL ERRINT ( '#', BODY                         )
            CALL SIGERR ( 'SPICE(INVALIDBODYCODE)'          )
            CALL CHKOUT ( 'T_PCKANGD'                        )
            RETURN

         END IF



      ELSE IF ( BODY/100 .EQ. 9 ) THEN


        IF ( BODY .EQ. 901 ) THEN

            A0  =    132.993D0

            D0  =     -6.163D0

            W   =    122.695D0   +   56.3625225D0 * D



         ELSE

            CALL SETMSG ( 'Body code # was not recognized.' )
            CALL ERRINT ( '#', BODY                         )
            CALL SIGERR ( 'SPICE(INVALIDBODYCODE)'          )
            CALL CHKOUT ( 'T_PCKANGD'                        )
            RETURN

         END IF


      ELSE IF ( BODY .EQ. 2000001 ) THEN
!
!        Ceres.
!
         A0  =     291.418D0

         D0  =      66.764D0

         W   =     170.650D0  +  952.1532D0 * D



      ELSE IF ( BODY .EQ. 2000002 ) THEN
!
!        Pallas.
!
         A0  =      33.D0

         D0  =      -3.D0

         W   =      38.D0  + 1105.8036D0 * D


      ELSE IF ( BODY .EQ. 2000004 ) THEN
!
!        Vesta.
!
         A0  =     309.031D0

         D0  =      42.235D0

         W   =     285.39D0   +   1617.3329428D0 * D


      ELSE IF ( BODY .EQ. 2000021 ) THEN
!
!        Lutetia.
!
         A0  =      52.D0

         D0  =      12.D0

         W   =      94.D0  +   1057.7515D0  *  D


      ELSE IF ( BODY .EQ. 2000052 ) THEN
!
!        Europa.
!
         A0  =     257.D0

         D0  =      12.D0

         W   =      55.D0  +   1534.6472187D0  *  D


      ELSE IF ( BODY .EQ. 2431010 ) THEN
!
!        Ida.
!
         A0  =     168.76D0

         D0  =     -87.12D0

         W   =     274.05D0  +  1864.6280070D0 * D


      ELSE IF ( BODY .EQ. 2000433 ) THEN
!
!        Eros.
!
         A0  =      11.35D0

         D0  =      17.22D0

         W   =     326.07D0  +  1639.38864745D0 * D



      ELSE IF ( BODY .EQ. 2000511 ) THEN
!
!        Davida.
!
         A0  =     297.D0

         D0  =       5.D0

         W   =     268.1D0 +  1684.4193549D0 * D


      ELSE IF ( BODY .EQ. 9511010 ) THEN
!
!        Gaspra.
!
         A0  =       9.47D0

         D0  =      26.70D0

         W   =      83.67D0  +  1226.9114850D0 * D


      ELSE IF ( BODY .EQ. 2002867 ) THEN
!
!        Steins.
!
         A0  =      91.D0

         D0  =     -62.D0

         W   =     321.76D0  +  1428.09917D0 * D

      ELSE IF ( BODY .EQ. 2025143 ) THEN
!
!        Itokawa.
!
         A0  =      90.53D0

         D0  =     -66.30D0

         W   =     000.D0    +   712.143D0 * D


      ELSE IF ( BODY .EQ. 1000093 ) THEN
!
!        Tempel 1.
!
!
!           2005-07-04 05:45:38.4 TDB (DI impact):
!
!              W = 109.7  W1 = 211.849/d
!
!           2011-02-15 04:40:18.6 TDB (Stardust NExT c/a):
!
         A0  =      255.D0

         D0  =       64.5D0

         W   =       69.2D0  +  212.807D0 * D


      ELSE IF ( BODY .EQ. 1000005 ) THEN
!
!        19/P Borrelly.
!
         A0  =      218.5D0

         D0  =      -12.5D0

         W   =      000.D0    +  324.3D0 * D


      ELSE IF ( BODY .EQ. 1000012 ) THEN
!
!        67P/Churyumov-Gerasimenko
!
!           2014-03-23 : 2014-09-03
!
         A0  =       69.54D0

         D0  =       64.11D0

         W   =      114.69D0    +  696.543884683D0 * D




      ELSE

         CALL SETMSG ( 'Body code # was not recognized.' )
         CALL ERRINT ( '#', BODY                         )
         CALL SIGERR ( 'SPICE(INVALIDBODYCODE)'          )
         CALL CHKOUT ( 'T_PCKANGD'                        )
         RETURN


      END IF




      RA   =  RPD() * A0
      DEC  =  RPD() * D0
      PM   =  RPD() * W


      CALL CHKOUT ( 'T_PCKANGD')
      RETURN






!$Procedure T_PCKRADD ( Text PCK body radii )

      ENTRY T_PCKRADD ( BODY, RADII )

! $ Abstract
!
!     Return reference ellipsoid radii provided for a specified
!     extended body by the IAU report [1].
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
!     PCK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     INTEGER               BODY
!     DOUBLE PRECISION      RADII  ( 3 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   BODY for which radii are to be returned.
!     RADII      O   Radii of reference ellipsoid for BODY.
!
! $ Detailed_Input
!
!     BODY     is the NAIF ID code of the body for radii
!              are to be returned.
!
! $ Detailed_Output
!
!     RADII    are the three radii of the reference ellipsoid
!              associated with the object designated by BODY.
!              This radii are provided by the IAU report [1].
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an unrecognized ID code is provided via the input argument
!         BODY, the error SPICE(INVALIDBODYCODE) is signaled.
!
! $ Files
!
!     The data provided by this routine are meant to be consistent with
!     the current IAU report [1] and should be consistent with the NAIF
!     text PCK
!
!        pck00009.tpc
!
!     This routine must be updated along with each NAIF text PCK update.
!
! $ Particulars
!
!     This routine supports comparison of reference ellipsoid radii
!     provided by the current NAIF text PCK against those provided by
!     the IAU report [1]. These tests serve to minimize the chance of
!     transcription error when a new PCK version is created. Entering
!     the PCK constants both in the new PCK file and in T_PCKRADD
!     enables double-checking of the constants.
!
! $ Examples
!
!     See use in F_RDPCK.
!
! $ Restrictions
!
!     1)  This routine must be kept consistent with the current IAU
!         report [1].
!
! $ Literature_References
!
!     [1]  Archinal, B.A., Acton, C.H., A'Hearn, M.F., Conrad, A.,
!          Consolmagno, G.J., Duxbury, T., Hestroffer, D., Hilton,
!          J.L., Jorda, L., Kirk, R.L., Klinoner, S.A., McCarthy, D.,
!          Meech, K., Oberst, J., Ping., J., Seidelmann, P.K., Tholen,
!          D.J., Thomas, P.C., and Williams, I.P., "Report of the IAU
!          Working Group on Cartographic Coordinates and Rotational
!          elements: 2015," Celest Mech Dyn Astr (2018) 130:22
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    Test Utilities Version 4.0.0, 30-MAY-2021 (NJB) (JDR)
!
!        Updated to reflect data from the 2015 IAU report.
!
!        Edited the header to comply with NAIF standard.
!
!-    Test Utilities Version 3.0.0, 29-JAN-2011 (NJB)
!
!        Updated to reflect data from the 2009 IAU report.
!
!-    Test Utilities Version 2.0.0, 10-FEB-2010 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


      CALL CHKIN( 'T_PCKRADD' )


      IF ( BODY .EQ. 10 ) THEN

         CALL VPACK ( 695700D0, 695700D0, 695700D0,  RADII )

      ELSE IF ( BODY .EQ. 199 ) THEN

         CALL VPACK ( 2440.53D0, 2440.53D0, 2438.26D0, RADII )

      ELSE IF ( BODY .EQ. 299 ) THEN

         CALL VPACK ( 6051.8D0, 6051.8D0, 6051.8D0,  RADII )

      ELSE IF ( BODY .EQ. 399 ) THEN

         CALL VPACK ( 6378.1366D0,  6378.1366D0,  6356.7519D0,  RADII )

      ELSE IF ( BODY .EQ. 499 ) THEN

         CALL VPACK ( 3396.19D0,  3396.19D0,  3376.20D0,  RADII )

      ELSE IF ( BODY .EQ. 599 ) THEN

         CALL VPACK ( 71492D0,  71492D0,  66854D0,  RADII )

      ELSE IF ( BODY .EQ. 699 ) THEN

         CALL VPACK ( 60268D0,  60268D0,  54364D0,  RADII )

      ELSE IF ( BODY .EQ. 799 ) THEN

         CALL VPACK ( 25559D0,  25559D0,  24973D0,  RADII )

      ELSE IF ( BODY .EQ. 899 ) THEN

         CALL VPACK ( 24764D0,  24764D0,  24341D0,  RADII )

      ELSE IF ( BODY .EQ. 999 ) THEN

         CALL VPACK ( 1188.3D0, 1188.3D0, 1188.3D0, RADII )



      ELSE IF ( BODY .EQ. 301 ) THEN

         CALL VPACK ( 1737.4D0,  1737.4D0,  1737.4D0,  RADII )



      ELSE IF ( BODY .EQ. 401 ) THEN

         CALL VPACK ( 13.0D0,  11.4D0,  9.1D0,  RADII )

      ELSE IF ( BODY .EQ. 402 ) THEN

         CALL VPACK ( 7.8D0,  6.0D0,  5.1D0,  RADII )



      ELSE IF ( BODY .EQ. 501 ) THEN

         CALL VPACK ( 1829.4D0,  1819.4D0,  1815.7D0,  RADII )

      ELSE IF ( BODY .EQ. 502 ) THEN

         CALL VPACK ( 1562.6D0,  1560.3D0,  1559.5D0,  RADII )

      ELSE IF ( BODY .EQ. 503 ) THEN

         CALL VPACK ( 2631.2D0,  2631.2D0,  2631.2D0,  RADII )

      ELSE IF ( BODY .EQ. 504 ) THEN

         CALL VPACK ( 2410.3D0,  2410.3D0,  2410.3D0,  RADII )

      ELSE IF ( BODY .EQ. 505 ) THEN

         CALL VPACK ( 125D0,  73D0,  64D0,  RADII )

      ELSE IF ( BODY .EQ. 506 ) THEN

         CALL VPACK ( 85D0,  85D0,  85D0,  RADII )

      ELSE IF ( BODY .EQ. 507 ) THEN

         CALL VPACK ( 40D0,  40D0,  40D0,  RADII )

      ELSE IF ( BODY .EQ. 508 ) THEN

         CALL VPACK ( 18D0,  18D0,  18D0,  RADII )

      ELSE IF ( BODY .EQ. 509 ) THEN

         CALL VPACK ( 14D0,  14D0,  14D0,  RADII )

      ELSE IF ( BODY .EQ. 510 ) THEN

         CALL VPACK ( 12D0,  12D0,  12D0,  RADII )

      ELSE IF ( BODY .EQ. 511 ) THEN

         CALL VPACK ( 15D0,  15D0,  15D0,  RADII )

      ELSE IF ( BODY .EQ. 512 ) THEN

         CALL VPACK ( 10D0,  10D0,  10D0,  RADII )

      ELSE IF ( BODY .EQ. 513 ) THEN

         CALL VPACK (  5D0,   5D0,   5D0,  RADII )

      ELSE IF ( BODY .EQ. 514 ) THEN

         CALL VPACK ( 58D0,  49D0,  42D0,  RADII )

      ELSE IF ( BODY .EQ. 515 ) THEN

         CALL VPACK ( 10D0,  8D0,  7D0,  RADII )

      ELSE IF ( BODY .EQ. 516 ) THEN

         CALL VPACK ( 30D0,  20D0,  17D0,  RADII )




      ELSE IF ( BODY .EQ. 601 ) THEN

         CALL VPACK ( 207.8D0,  196.7D0,  190.6D0,  RADII )

      ELSE IF ( BODY .EQ. 602 ) THEN

         CALL VPACK ( 256.6D0,  251.4D0,  248.3D0,  RADII )

      ELSE IF ( BODY .EQ. 603 ) THEN

         CALL VPACK ( 538.4D0,  528.3D0,  526.3D0,  RADII )

      ELSE IF ( BODY .EQ. 604 ) THEN

         CALL VPACK ( 563.4D0,  561.3D0,  559.6D0,  RADII )

      ELSE IF ( BODY .EQ. 605 ) THEN

         CALL VPACK ( 765.0D0,  763.1D0,  762.4D0,  RADII )

      ELSE IF ( BODY .EQ. 606 ) THEN

         CALL VPACK ( 2575.15D0,  2574.78D0,  2574.47D0,  RADII )

      ELSE IF ( BODY .EQ. 607 ) THEN

         CALL VPACK ( 180.1D0,  133.0D0,  102.7D0,  RADII )

      ELSE IF ( BODY .EQ. 608 ) THEN

         CALL VPACK ( 745.7D0,  745.7D0,  712.1D0,  RADII )

      ELSE IF ( BODY .EQ. 609 ) THEN

         CALL VPACK ( 109.4D0,  108.5D0,  101.8D0,  RADII )

      ELSE IF ( BODY .EQ. 610 ) THEN

         CALL VPACK ( 101.7D0,   93.0D0,   76.3D0,  RADII )

      ELSE IF ( BODY .EQ. 611 ) THEN

         CALL VPACK (  64.9D0,   57.3D0,   53.0D0,  RADII )

      ELSE IF ( BODY .EQ. 612 ) THEN

         CALL VPACK (  22.5D0,   19.6D0,   13.3D0,  RADII )

      ELSE IF ( BODY .EQ. 613 ) THEN

         CALL VPACK (  16.3D0,   11.8D0,    9.8D0,  RADII )

      ELSE IF ( BODY .EQ. 614 ) THEN

         CALL VPACK (  15.3D0,    9.3D0,    6.3D0,  RADII )

      ELSE IF ( BODY .EQ. 615 ) THEN

         CALL VPACK (  20.5D0,   17.8D0,    9.4D0,  RADII )

      ELSE IF ( BODY .EQ. 616 ) THEN

         CALL VPACK (  68.2D0,   41.6D0,   28.2D0,  RADII )

      ELSE IF ( BODY .EQ. 617 ) THEN

         CALL VPACK (  52.2D0,   40.8D0,   31.5D0,  RADII )

      ELSE IF ( BODY .EQ. 618 ) THEN

         CALL VPACK (  17.2D0,   15.4D0,  10.4D0,  RADII )

      ELSE IF ( BODY .EQ. 632 ) THEN

         CALL VPACK (   1.94D0,   1.29D0,  1.21D0,  RADII )

      ELSE IF ( BODY .EQ. 633 ) THEN

         CALL VPACK (   2.88D0,   2.08D0,  1.8D0,   RADII )

      ELSE IF ( BODY .EQ. 634 ) THEN

         CALL VPACK (   1.5D0,    1.2D0,   1.0D0,  RADII )

      ELSE IF ( BODY .EQ. 635 ) THEN

         CALL VPACK (   4.6D0,    4.5D0,   2.8D0,  RADII )

      ELSE IF ( BODY .EQ. 649 ) THEN

         CALL VPACK (   0.5D0,    0.5D0,   0.5D0,  RADII )

      ELSE IF ( BODY .EQ. 653 ) THEN

         CALL VPACK (   0.7D0,     0.25D0, 0.2D0,  RADII )


      ELSE IF ( BODY .EQ. 701 ) THEN

         CALL VPACK ( 581.1D0,  577.9D0,  577.7D0,  RADII )

      ELSE IF ( BODY .EQ. 702 ) THEN

         CALL VPACK ( 584.7D0,  584.7D0,  584.7D0,  RADII )

      ELSE IF ( BODY .EQ. 703 ) THEN

         CALL VPACK ( 788.9D0,  788.9D0,  788.9D0,  RADII )

      ELSE IF ( BODY .EQ. 704 ) THEN

         CALL VPACK ( 761.4D0,  761.4D0,  761.4D0,  RADII )

      ELSE IF ( BODY .EQ. 705 ) THEN

         CALL VPACK ( 240.4D0,  234.2D0,  232.9D0,  RADII )

      ELSE IF ( BODY .EQ. 706 ) THEN

         CALL VPACK (  13D0,     13D0,     13D0,  RADII )

      ELSE IF ( BODY .EQ. 707 ) THEN

         CALL VPACK (  15D0,     15D0,     15D0,  RADII )

      ELSE IF ( BODY .EQ. 708 ) THEN

         CALL VPACK (  21D0,     21D0,     21D0,  RADII )

      ELSE IF ( BODY .EQ. 709 ) THEN

         CALL VPACK (  31D0,     31D0,     31D0,  RADII )

      ELSE IF ( BODY .EQ. 710 ) THEN

         CALL VPACK (  27D0,     27D0,     27D0,  RADII )

      ELSE IF ( BODY .EQ. 711 ) THEN

         CALL VPACK (  42D0,     42D0,     42D0,  RADII )

      ELSE IF ( BODY .EQ. 712 ) THEN

         CALL VPACK (  54D0,     54D0,     54D0,  RADII )

      ELSE IF ( BODY .EQ. 713 ) THEN

         CALL VPACK (  27D0,     27D0,     27D0,  RADII )

      ELSE IF ( BODY .EQ. 714 ) THEN

         CALL VPACK (  33D0,     33D0,     33D0,  RADII )

      ELSE IF ( BODY .EQ. 715 ) THEN

         CALL VPACK (  77D0,     77D0,     77D0,  RADII )




      ELSE IF ( BODY .EQ. 801 ) THEN

         CALL VPACK ( 1352.6D0,  1352.6D0,  1352.6D0,   RADII )

       ELSE IF ( BODY .EQ. 802 ) THEN

         CALL VPACK (    170D0,     170D0,     170D0,  RADII )

      ELSE IF ( BODY .EQ. 803 ) THEN

         CALL VPACK (     29D0,      29D0,      29D0,  RADII )

      ELSE IF ( BODY .EQ. 804 ) THEN

         CALL VPACK (     40D0,      40D0,      40D0,  RADII )

      ELSE IF ( BODY .EQ. 805 ) THEN

         CALL VPACK (     74D0,      74D0,      74D0,  RADII )

      ELSE IF ( BODY .EQ. 806 ) THEN

         CALL VPACK (     79D0,      79D0,      79D0,  RADII )

      ELSE IF ( BODY .EQ. 807 ) THEN

         CALL VPACK (     96D0,      96D0,      96D0,  RADII )

      ELSE IF ( BODY .EQ. 808 ) THEN

         CALL VPACK (   218D0,      208D0,     201D0,  RADII )




      ELSE IF ( BODY .EQ. 901 ) THEN

         CALL VPACK ( 606D0,  606D0,  606D0,  RADII )




      ELSE IF ( BODY .EQ. 1000005 ) THEN
!
!        Borrelly. (Effective radius)
!
         CALL VPACK ( 4.22D0,  4.22D0,  4.22D0,  RADII )


      ELSE IF ( BODY .EQ. 1000012  ) THEN
!
!        Churyumov-Gerasimenko.
!
         CALL VPACK ( 2.4D0,   1.55D0, 1.2D0,   RADII )


      ELSE IF ( BODY .EQ. 1000036  ) THEN
!
!        Halley.
!
         CALL VPACK ( 8.0D0,   4.0D0,  4.0D0,   RADII )


      ELSE IF ( BODY .EQ. 1000093  ) THEN
!
!        Tempel 1.  (Effective radius)
!
         CALL VPACK ( 3.0D0,  3.0D0,  3.0D0,  RADII )


      ELSE IF ( BODY .EQ. 1000041  ) THEN
!
!        Hartley 2.
!
!         CALL VPACK ( 0.34D0, 1.16D0, 0.34, RADII )

         CALL VPACK ( 0.58D0, 0.58D0, 0.58D0,  RADII )


      ELSE IF ( BODY .EQ. 1000107  ) THEN
!
!        Wild 2.
!
         CALL VPACK ( 2.7D0,  1.9D0,   1.5D0,  RADII )


      ELSE IF ( BODY .EQ. 2000001 ) THEN
!
!        Ceres.
!
         CALL VPACK ( 487.3D0,  487.3D0,   446.0D0,   RADII )


      ELSE IF ( BODY .EQ. 2000004 ) THEN
!
!        Vesta.
!
         CALL VPACK ( 289D0,  280D0,  229D0,  RADII )


      ELSE IF ( BODY .EQ. 2000021 ) THEN
!
!        Lutetia.
!
         CALL VPACK (  62.0D0,  50.5D0,  46.5D0,  RADII )


      ELSE IF ( BODY .EQ. 2000016 ) THEN
!
!        Psyche.
!
         CALL VPACK ( 139.5D0, 116.D0, 94.5D0,  RADII )


      ELSE IF ( BODY .EQ. 2000052 ) THEN
!
!        Europa.
!
         CALL VPACK ( 189.5D0, 165.D0, 124.5D0, RADII )


      ELSE IF ( BODY .EQ. 2000253 ) THEN
!
!        Mathilde.
!
         CALL VPACK ( 33D0,  24D0,  23D0,  RADII )


      ELSE IF ( BODY .EQ. 2000433 ) THEN
!
!        Eros.
!
         CALL VPACK ( 17.0D0, 5.5D0, 5.5D0,  RADII )


      ELSE IF ( BODY .EQ. 2000511 ) THEN
!
!        Davida.
!
         CALL VPACK ( 180D0,  147D0,  127D0,  RADII )


      ELSE IF ( BODY .EQ. 2002867 ) THEN
!
!        Steins.
!
         CALL VPACK ( 3.24D0,  2.73D0,  2.04D0,  RADII )


      ELSE IF ( BODY .EQ. 2004179 ) THEN
!
!        Toutatis.
!
         CALL VPACK ( 2.13D0,  1.015D0,  0.85D0,  RADII )


      ELSE IF ( BODY .EQ. 2025143 ) THEN
!
!        Itokawa.
!
         CALL VPACK ( 0.268D0,  0.147D0,  0.104D0,  RADII )


      ELSE IF ( BODY .EQ. 2431010 ) THEN
!
!        Ida.
!
         CALL VPACK ( 26.8D0,  12.0D0,  7.6D0,  RADII )


      ELSE IF ( BODY .EQ. 9511010 ) THEN
!
!        Gaspra.
!
         CALL VPACK ( 9.1D0,  5.2D0,  4.4D0,  RADII )


      ELSE

         CALL SETMSG ( 'Body code # was not recognized.' )
         CALL ERRINT ( '#', BODY                         )
         CALL SIGERR ( 'SPICE(INVALIDBODYCODE)'          )
         CALL CHKOUT ( 'T_PCKRADD'                        )
         RETURN


      END IF


      CALL CHKOUT ( 'T_PCKRADD' )
      RETURN
      END
