! $ Abstract
!
!    This program performs a set of standard tests on the SPICE
!    product spicelib.
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
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Files
!
!     This program creates two log files.
!
!     The first is the test log whose name follows the pattern
!     spice{0-9}{0-9}.log.  The second is an ERROR log or
!     a pass log.
!
!     The pass log follows the pattern pass{0-9}{0-9}{0-9}{0-9}.log
!     The error log follows the pattern ERROR{0-9}{0-9}{0-9}.LOG
!
!     Any kernel files needed by this program are created by the
!     program.
!
! $ Particulars
!
!     To run this test, execute the program
!
!       tspice
!
!     You may optionally specify that the program use one of the
!     command options:
!
!        -c  logs results of individual test cases
!
!        -v  logs every test comparison performed.  You should
!            probably never use this option.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!     Created  on Fri Dec 31 18:50:56 PST 2021 as part
!     of an automated process for delivering
!     the test programs. --- Boris Semeov
!
!
!
!-&

      PROGRAM TSPICE
      CHARACTER*(1024)      CMLINE
      LOGICAL               OK

!
!     Set up for testing.  Establish the name of the log file and
!     the version number of the test program.
!
      CALL GETCML ( CMLINE )
      CALL TSETUP ( CMLINE, 'spice{0-9}{0-9}.log', '1.0.0' )
      CALL F_AAAAPHSH (OK)
      CALL F_AB (OK)
      CALL F_ASNACSN (OK)
      CALL F_AZL (OK)
      CALL F_AZLCPV (OK)
      CALL F_BADKPV (OK)
      CALL F_BODCOD (OK)
      CALL F_BODVAR (OK)
      CALL F_CCIFRM (OK)
      CALL F_CHBIGR (OK)
      CALL F_CHBSHV (OK)
      CALL F_CHGIRF (OK)
      CALL F_CHGRFX (OK)
      CALL F_CK02 (OK)
      CALL F_CK03 (OK)
      CALL F_CK05 (OK)
      CALL F_CK06 (OK)
      CALL F_CKBSR (OK)
      CALL F_CKCOV (OK)
      CALL F_CKFROT (OK)
      CALL F_CKFXFM (OK)
      CALL F_CKGP (OK)
      CALL F_CKMETA (OK)
      CALL F_CKW01 (OK)
      CALL F_CKW02 (OK)
      CALL F_CKW03 (OK)
      CALL F_CONVRT (OK)
      CALL F_CRDCNV (OK)
      CALL F_CYIP (OK)
      CALL F_DAFAH (OK)
      CALL F_DAFANA (OK)
      CALL F_DAFNN (OK)
      CALL F_DAS (OK)
      CALL F_DASA2L (OK)
      CALL F_DASCUD (OK)
      CALL F_DASFR (OK)
      CALL F_DASMUL (OK)
      CALL F_DDHCLS (OK)
      CALL F_DDHCLU (OK)
      CALL F_DDHF2H (OK)
      CALL F_DDHFNH (OK)
      CALL F_DDHFTSIZE (OK)
      CALL F_DDHGSD (OK)
      CALL F_DDHGTU (OK)
      CALL F_DDHHLU (OK)
      CALL F_DDHISN (OK)
      CALL F_DDHIVF (OK)
      CALL F_DDHLUH (OK)
      CALL F_DDHNFO (OK)
      CALL F_DDHOPN (OK)
      CALL F_DDHPPF (OK)
      CALL F_DDHRCM (OK)
      CALL F_DDHRMU (OK)
      CALL F_DDHUNL (OK)
      CALL F_DHFA (OK)
      CALL F_DLA (OK)
      CALL F_DNEARP (OK)
      CALL F_DPFMT (OK)
      CALL F_DPSTRF (OK)
      CALL F_DSK02 (OK)
      CALL F_DSKKPR (OK)
      CALL F_DSKMI2 (OK)
      CALL F_DSKOBJ (OK)
      CALL F_DSKRB2 (OK)
      CALL F_DSKTOL (OK)
      CALL F_DSKW02 (OK)
      CALL F_DSKX02 (OK)
      CALL F_DSKXSI (OK)
      CALL F_DSKXV (OK)
      CALL F_DVOPS (OK)
      CALL F_DVSEP (OK)
      CALL F_DWPOOL (OK)
      CALL F_DYN01 (OK)
      CALL F_DYN02 (OK)
      CALL F_DYN03 (OK)
      CALL F_DYN04 (OK)
      CALL F_DYN05 (OK)
      CALL F_DYN06 (OK)
      CALL F_DYN07 (OK)
      CALL F_DYN08 (OK)
      CALL F_EDNMPT (OK)
      CALL F_EDPNT (OK)
      CALL F_EK01 (OK)
      CALL F_EK02 (OK)
      CALL F_EK03 (OK)
      CALL F_EK04 (OK)
      CALL F_EQNCPV (OK)
      CALL F_ET2LST (OK)
      CALL F_ET2UTC (OK)
      CALL F_ETCAL (OK)
      CALL F_EULER (OK)
      CALL F_FNDCMP (OK)
      CALL F_FOVRAY (OK)
      CALL F_FOVTRG (OK)
      CALL F_FRAMEX (OK)
      CALL F_FRFTCH (OK)
      CALL F_FRMCHG (OK)
      CALL F_FRMGET (OK)
      CALL F_GE01 (OK)
      CALL F_GETFOV (OK)
      CALL F_GETFV2 (OK)
      CALL F_GFBAIL (OK)
      CALL F_GFBIRP (OK)
      CALL F_GFDIRP (OK)
      CALL F_GFDIST (OK)
      CALL F_GFEVNT (OK)
      CALL F_GFFOVE (OK)
      CALL F_GFILUM (OK)
      CALL F_GFLORP (OK)
      CALL F_GFOCCE (OK)
      CALL F_GFOCLT (OK)
      CALL F_GFPA (OK)
      CALL F_GFPCRP (OK)
      CALL F_GFPOSC (OK)
      CALL F_GFREFN (OK)
      CALL F_GFRFOV (OK)
      CALL F_GFRPRT (OK)
      CALL F_GFRR (OK)
      CALL F_GFSCRP (OK)
      CALL F_GFSEP (OK)
      CALL F_GFSNTC (OK)
      CALL F_GFSPRP (OK)
      CALL F_GFSTEP (OK)
      CALL F_GFSUBC (OK)
      CALL F_GFTFOV (OK)
      CALL F_GFUDB (OK)
      CALL F_GFUDS (OK)
      CALL F_GFXCRP (OK)
      CALL F_HRMITE (OK)
      CALL F_ILLUMF (OK)
      CALL F_ILLUMG (OK)
      CALL F_ILUMIN (OK)
      CALL F_INCNSG (OK)
      CALL F_INELPL (OK)
      CALL F_INRYPL (OK)
      CALL F_INSANG (OK)
      CALL F_INSERT (OK)
      CALL F_IOVCMP (OK)
      CALL F_JUL2GR (OK)
      CALL F_KEEPER (OK)
      CALL F_KPBUG (OK)
      CALL F_KPSOLV (OK)
      CALL F_LAGRNG (OK)
      CALL F_LATSRF (OK)
      CALL F_LIMBPT (OK)
      CALL F_LOCATI (OK)
      CALL F_LPARSE (OK)
      CALL F_LS (OK)
      CALL F_LTIME (OK)
      CALL F_M2Q (OK)
      CALL F_MATRIX3 (OK)
      CALL F_MATRIXG (OK)
      CALL F_MOVED (OK)
      CALL F_NEARPT (OK)
      CALL F_NNEK01 (OK)
      CALL F_NNEK03 (OK)
      CALL F_NNEK04 (OK)
      CALL F_NPEDLN (OK)
      CALL F_NPELPT (OK)
      CALL F_NPLNPT (OK)
      CALL F_NPSGPT (OK)
      CALL F_OCCULT (OK)
      CALL F_OSCELT (OK)
      CALL F_OSCLTX (OK)
      CALL F_PCK20 (OK)
      CALL F_PCKBSR (OK)
      CALL F_PCKBUF (OK)
      CALL F_PCKCOV (OK)
      CALL F_PGR (OK)
      CALL F_PHASEQ (OK)
      CALL F_PLN (OK)
      CALL F_PLT (OK)
      CALL F_PLTNP (OK)
      CALL F_POLYDS (OK)
      CALL F_POOL (OK)
      CALL F_PRDFRM (OK)
      CALL F_PRJP (OK)
      CALL F_PXFORM (OK)
      CALL F_PXFRM2 (OK)
      CALL F_Q2M (OK)
      CALL F_QDERIV (OK)
      CALL F_QUAT (OK)
      CALL F_RC2GRD (OK)
      CALL F_RDPCK (OK)
      CALL F_RDPCKD (OK)
      CALL F_REFCHG (OK)
      CALL F_REGLON (OK)
      CALL F_REPMX (OK)
      CALL F_ROTGET (OK)
      CALL F_SAELGV (OK)
      CALL F_SBF (OK)
      CALL F_SCLK (OK)
      CALL F_SCLK0 (OK)
      CALL F_SCLK1 (OK)
      CALL F_SCTRAN (OK)
      CALL F_SEPOOL (OK)
      CALL F_SGMETA (OK)
      CALL F_SHARPR (OK)
      CALL F_SINCPT (OK)
      CALL F_SLICE (OK)
      CALL F_SPK01 (OK)
      CALL F_SPK02 (OK)
      CALL F_SPK03 (OK)
      CALL F_SPK05 (OK)
      CALL F_SPK08 (OK)
      CALL F_SPK09 (OK)
      CALL F_SPK10 (OK)
      CALL F_SPK12 (OK)
      CALL F_SPK13 (OK)
      CALL F_SPK14 (OK)
      CALL F_SPK17 (OK)
      CALL F_SPK18 (OK)
      CALL F_SPK19 (OK)
      CALL F_SPK20 (OK)
      CALL F_SPK21 (OK)
      CALL F_SPKAPO (OK)
      CALL F_SPKAPP (OK)
      CALL F_SPKBSR (OK)
      CALL F_SPKCOR (OK)
      CALL F_SPKCOV (OK)
      CALL F_SPKCPV (OK)
      CALL F_SPKE15 (OK)
      CALL F_SPKEZ (OK)
      CALL F_SPKEZP (OK)
      CALL F_SPKF15 (OK)
      CALL F_SPKGEO (OK)
      CALL F_SPKGPS (OK)
      CALL F_SPKGPX (OK)
      CALL F_SPKGXC (OK)
      CALL F_SPKPDS (OK)
      CALL F_SPKPVN (OK)
      CALL F_SPKS19 (OK)
      CALL F_SPKSPV (OK)
      CALL F_SPKW01 (OK)
      CALL F_SRFNRM (OK)
      CALL F_SRFTRN (OK)
      CALL F_SRFXPT (OK)
      CALL F_ST00 (OK)
      CALL F_STLABX (OK)
      CALL F_STMP03 (OK)
      CALL F_STPOOL (OK)
      CALL F_STR2ET (OK)
      CALL F_STRING (OK)
      CALL F_SUBPNT (OK)
      CALL F_SUBSLR (OK)
      CALL F_SURFNM (OK)
      CALL F_SURFPV (OK)
      CALL F_SWAPAC (OK)
      CALL F_SWAPAD (OK)
      CALL F_SWAPAI (OK)
      CALL F_SXFORM (OK)
      CALL F_SYMTBC (OK)
      CALL F_SYMTBD (OK)
      CALL F_SYMTBI (OK)
      CALL F_T_URAND (OK)
      CALL F_TABTXT (OK)
      CALL F_TANGPT0 (OK)
      CALL F_TANGPT1 (OK)
      CALL F_TANGPT2 (OK)
      CALL F_TANGPT3 (OK)
      CALL F_TANGPTW (OK)
      CALL F_TCHECK (OK)
      CALL F_TERM (OK)
      CALL F_TERMPT (OK)
      CALL F_TEXPYR (OK)
      CALL F_TIMCVR (OK)
      CALL F_TIMDEF (OK)
      CALL F_TIMOUT (OK)
      CALL F_TKFRAM (OK)
      CALL F_TLE (OK)
      CALL F_TNCNSG (OK)
      CALL F_TPARSE (OK)
      CALL F_TPARTV1 (OK)
      CALL F_TPARTV2 (OK)
      CALL F_TRGSEP (OK)
      CALL F_TSTCK3 (OK)
      CALL F_TTRANS (OK)
      CALL F_TWOVXF (OK)
      CALL F_UTC2ET (OK)
      CALL F_VECTOR3 (OK)
      CALL F_VECTORG (OK)
      CALL F_VOXEL (OK)
      CALL F_VSTRNG (OK)
      CALL F_WIN (OK)
      CALL F_XDDA (OK)
      CALL F_XFMSTA (OK)
      CALL F_XFNEUL (OK)
      CALL F_XFRAV (OK)
      CALL F_XLATED (OK)
      CALL F_XLATEI (OK)
      CALL F_ZZASC1 (OK)
      CALL F_ZZASC2 (OK)
      CALL F_ZZASRYEL (OK)
      CALL F_ZZBDIN (OK)
      CALL F_ZZBDKR (OK)
      CALL F_ZZBDTN (OK)
      CALL F_ZZBDTRN (OK)
      CALL F_ZZBODS (OK)
      CALL F_ZZBODS2C (OK)
      CALL F_ZZBODVCD (OK)
      CALL F_ZZBQUAD (OK)
      CALL F_ZZCKE06 (OK)
      CALL F_ZZCNQUAD (OK)
      CALL F_ZZCORSXF (OK)
      CALL F_ZZCTR (OK)
      CALL F_ZZCXBRUT (OK)
      CALL F_ZZDASGRC (OK)
      CALL F_ZZDDHNFC (OK)
      CALL F_ZZDGDR (OK)
      CALL F_ZZDGFR (OK)
      CALL F_ZZDGSR (OK)
      CALL F_ZZDM (OK)
      CALL F_ZZDSKBSR (OK)
      CALL F_ZZDSKBUX (OK)
      CALL F_ZZDSKSEL (OK)
      CALL F_ZZDSKSGR (OK)
      CALL F_ZZDSKSGX (OK)
      CALL F_ZZDSKSPH (OK)
      CALL F_ZZEDTMPT (OK)
      CALL F_ZZELLBDS (OK)
      CALL F_ZZELNAXX (OK)
      CALL F_ZZELVUPY0 (OK)
      CALL F_ZZFDAT (OK)
      CALL F_ZZFGEO (OK)
      CALL F_ZZFOVAXI (OK)
      CALL F_ZZGETELM (OK)
      CALL F_ZZGFCOQ (OK)
      CALL F_ZZGFCOST (OK)
      CALL F_ZZGFCOU (OK)
      CALL F_ZZGFCPRX (OK)
      CALL F_ZZGFCSLV (OK)
      CALL F_ZZGFDIQ (OK)
      CALL F_ZZGFDIU (OK)
      CALL F_ZZGFFVU (OK)
      CALL F_ZZGFILU (OK)
      CALL F_ZZGFLNG1 (OK)
      CALL F_ZZGFLNG2 (OK)
      CALL F_ZZGFLNG3 (OK)
      CALL F_ZZGFOCU (OK)
      CALL F_ZZGFPAU (OK)
      CALL F_ZZGFREL (OK)
      CALL F_ZZGFRELX (OK)
      CALL F_ZZGFRPWK (OK)
      CALL F_ZZGFRRU (OK)
      CALL F_ZZGFSOLV (OK)
      CALL F_ZZGFSOLVX (OK)
      CALL F_ZZGFSPQ (OK)
      CALL F_ZZGFSPU (OK)
      CALL F_ZZGFSSIN (OK)
      CALL F_ZZGFSSOB (OK)
      CALL F_ZZGFWSTS (OK)
      CALL F_ZZHSC (OK)
      CALL F_ZZHSI (OK)
      CALL F_ZZHULLAX (OK)
      CALL F_ZZILUSTA (OK)
      CALL F_ZZINLAT (OK)
      CALL F_ZZINLAT0 (OK)
      CALL F_ZZINPDT (OK)
      CALL F_ZZINPDT0 (OK)
      CALL F_ZZINREC (OK)
      CALL F_ZZINVELT (OK)
      CALL F_ZZLATBOX (OK)
      CALL F_ZZMKSPIN (OK)
      CALL F_ZZMSXF (OK)
      CALL F_ZZNAMFRM (OK)
      CALL F_ZZNOFCON (OK)
      CALL F_ZZNRMLON (OK)
      CALL F_ZZOCCED (OK)
      CALL F_ZZOCCED2 (OK)
      CALL F_ZZOCCED3 (OK)
      CALL F_ZZPDCMPL (OK)
      CALL F_ZZPDPLTC (OK)
      CALL F_ZZPDTBOX (OK)
      CALL F_ZZPLAT (OK)
      CALL F_ZZPTPL02 (OK)
      CALL F_ZZRAYBOX (OK)
      CALL F_ZZRECBOX (OK)
      CALL F_ZZRTNMAT (OK)
      CALL F_ZZRXR (OK)
      CALL F_ZZRYTELT (OK)
      CALL F_ZZRYTLAT (OK)
      CALL F_ZZRYTPDT (OK)
      CALL F_ZZRYTREC (OK)
      CALL F_ZZSC01 (OK)
      CALL F_ZZSEGBOX (OK)
      CALL F_ZZSEPQ (OK)
      CALL F_ZZSFXCOR (OK)
      CALL F_ZZSGLATX (OK)
      CALL F_ZZSINUTL (OK)
      CALL F_ZZSPKFUN (OK)
      CALL F_ZZSTELAB (OK)
      CALL F_ZZSWFFET (OK)
      CALL F_ZZSWFXFM (OK)
      CALL F_ZZTANSLV (OK)
      CALL F_ZZTANUTL (OK)
      CALL F_ZZTIME (OK)
      CALL F_ZZUTC (OK)
      CALL F_ZZVALCOR (OK)
      CALL F_ZZVRTPLT (OK)
      CALL F_ZZWIND2D (OK)

!
!     Close up the testing.
!
      CALL TCLOSE

      IF ( OK ) THEN
         CALL BYEBYE ( 'SUCCESS' )
      ELSE
         CALL BYEBYE ( 'FAILURE' )
      END IF

      END
