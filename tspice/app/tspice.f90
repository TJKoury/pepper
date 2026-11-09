! $ Abstract
!
!    This program performs a set of standard tests on the SPICE
!    product spicelib.
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
!        -d  debugging mode: files are not deleted between families
!
!        -q  suppress output to standard out
!
!        -l, --list
!            print the names of all test families, one per line,
!            and exit
!
!     Any other arguments are names of test families to run
!     (case-insensitive), e.g.
!
!        tspice F_SPKEZR F_CKGP
!
!     Selected families are run in the standard order. With no
!     family names, all families are run.
!
!     The exit status is 0 if every test case passed, 1 otherwise
!     (2 for invalid arguments).
!
!     tools/run_tspice.py uses this to run the families in parallel,
!     each in its own process and working directory.
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
!     PEPPER: added test family selection on the command line, the
!     --list option, and an exit status based on all families (the
!     original used the result of the last family only).
!
!     Created  on Fri Dec 31 18:50:56 PST 2021 as part
!     of an automated process for delivering
!     the test programs. --- Boris Semeov
!
!
!
!-&

      PROGRAM TSPICE
      IMPLICIT NONE

      INTEGER, PARAMETER :: NFAM = 400
      INTEGER, PARAMETER :: NAMLEN = 32

      CHARACTER(LEN=NAMLEN), PARAMETER :: FAMILY(NFAM) = [ CHARACTER(LEN=NAMLEN) :: &
         'F_AAAAPHSH', &
         'F_AB', &
         'F_ASNACSN', &
         'F_AZL', &
         'F_AZLCPV', &
         'F_BADKPV', &
         'F_BODCOD', &
         'F_BODVAR', &
         'F_CCIFRM', &
         'F_CHBIGR', &
         'F_CHBSHV', &
         'F_CHGIRF', &
         'F_CHGRFX', &
         'F_CK02', &
         'F_CK03', &
         'F_CK05', &
         'F_CK06', &
         'F_CKBSR', &
         'F_CKCOV', &
         'F_CKFROT', &
         'F_CKFXFM', &
         'F_CKGP', &
         'F_CKMETA', &
         'F_CKW01', &
         'F_CKW02', &
         'F_CKW03', &
         'F_CONVRT', &
         'F_CRDCNV', &
         'F_CYIP', &
         'F_DAFAH', &
         'F_DAFANA', &
         'F_DAFNN', &
         'F_DAS', &
         'F_DASA2L', &
         'F_DASCUD', &
         'F_DASFR', &
         'F_DASMUL', &
         'F_DDHCLS', &
         'F_DDHCLU', &
         'F_DDHF2H', &
         'F_DDHFNH', &
         'F_DDHFTSIZE', &
         'F_DDHGSD', &
         'F_DDHGTU', &
         'F_DDHHLU', &
         'F_DDHISN', &
         'F_DDHIVF', &
         'F_DDHLUH', &
         'F_DDHNFO', &
         'F_DDHOPN', &
         'F_DDHPPF', &
         'F_DDHRCM', &
         'F_DDHRMU', &
         'F_DDHUNL', &
         'F_DHFA', &
         'F_DLA', &
         'F_DNEARP', &
         'F_DPFMT', &
         'F_DPSTRF', &
         'F_DSK02', &
         'F_DSKKPR', &
         'F_DSKMI2', &
         'F_DSKOBJ', &
         'F_DSKRB2', &
         'F_DSKTOL', &
         'F_DSKW02', &
         'F_DSKX02', &
         'F_DSKXSI', &
         'F_DSKXV', &
         'F_DVOPS', &
         'F_DVSEP', &
         'F_DWPOOL', &
         'F_DYN01', &
         'F_DYN02', &
         'F_DYN03', &
         'F_DYN04', &
         'F_DYN05', &
         'F_DYN06', &
         'F_DYN07', &
         'F_DYN08', &
         'F_EDNMPT', &
         'F_EDPNT', &
         'F_EK01', &
         'F_EK02', &
         'F_EK03', &
         'F_EK04', &
         'F_EQNCPV', &
         'F_ET2LST', &
         'F_ET2UTC', &
         'F_ETCAL', &
         'F_EULER', &
         'F_FNDCMP', &
         'F_FOVRAY', &
         'F_FOVTRG', &
         'F_FRAMEX', &
         'F_FRFTCH', &
         'F_FRMCHG', &
         'F_FRMGET', &
         'F_GE01', &
         'F_GETFOV', &
         'F_GETFV2', &
         'F_GFBAIL', &
         'F_GFBIRP', &
         'F_GFDIRP', &
         'F_GFDIST', &
         'F_GFEVNT', &
         'F_GFFOVE', &
         'F_GFILUM', &
         'F_GFLORP', &
         'F_GFOCCE', &
         'F_GFOCLT', &
         'F_GFPA', &
         'F_GFPCRP', &
         'F_GFPOSC', &
         'F_GFREFN', &
         'F_GFRFOV', &
         'F_GFRPRT', &
         'F_GFRR', &
         'F_GFSCRP', &
         'F_GFSEP', &
         'F_GFSNTC', &
         'F_GFSPRP', &
         'F_GFSTEP', &
         'F_GFSUBC', &
         'F_GFTFOV', &
         'F_GFUDB', &
         'F_GFUDS', &
         'F_GFXCRP', &
         'F_HRMITE', &
         'F_ILLUMF', &
         'F_ILLUMG', &
         'F_ILUMIN', &
         'F_INCNSG', &
         'F_INELPL', &
         'F_INRYPL', &
         'F_INSANG', &
         'F_INSERT', &
         'F_IOVCMP', &
         'F_JUL2GR', &
         'F_KEEPER', &
         'F_KPBUG', &
         'F_KPSOLV', &
         'F_LAGRNG', &
         'F_LATSRF', &
         'F_LIMBPT', &
         'F_LOCATI', &
         'F_LPARSE', &
         'F_LS', &
         'F_LTIME', &
         'F_M2Q', &
         'F_MATRIX3', &
         'F_MATRIXG', &
         'F_MOVED', &
         'F_NEARPT', &
         'F_NNEK01', &
         'F_NNEK03', &
         'F_NNEK04', &
         'F_NPEDLN', &
         'F_NPELPT', &
         'F_NPLNPT', &
         'F_NPSGPT', &
         'F_OCCULT', &
         'F_OSCELT', &
         'F_OSCLTX', &
         'F_PCK20', &
         'F_PCKBSR', &
         'F_PCKBUF', &
         'F_PCKCOV', &
         'F_PGR', &
         'F_PHASEQ', &
         'F_PLN', &
         'F_PLT', &
         'F_PLTNP', &
         'F_POLYDS', &
         'F_POOL', &
         'F_PRDFRM', &
         'F_PRJP', &
         'F_PXFORM', &
         'F_PXFRM2', &
         'F_Q2M', &
         'F_QDERIV', &
         'F_QUAT', &
         'F_RC2GRD', &
         'F_RDPCK', &
         'F_RDPCKD', &
         'F_REFCHG', &
         'F_REGLON', &
         'F_REPMX', &
         'F_ROTGET', &
         'F_SAELGV', &
         'F_SBF', &
         'F_SCLK', &
         'F_SCLK0', &
         'F_SCLK1', &
         'F_SCTRAN', &
         'F_SEPOOL', &
         'F_SGMETA', &
         'F_SHARPR', &
         'F_SINCPT', &
         'F_SLICE', &
         'F_SPK01', &
         'F_SPK02', &
         'F_SPK03', &
         'F_SPK05', &
         'F_SPK08', &
         'F_SPK09', &
         'F_SPK10', &
         'F_SPK12', &
         'F_SPK13', &
         'F_SPK14', &
         'F_SPK17', &
         'F_SPK18', &
         'F_SPK19', &
         'F_SPK20', &
         'F_SPK21', &
         'F_SPKAPO', &
         'F_SPKAPP', &
         'F_SPKBSR', &
         'F_SPKCOR', &
         'F_SPKCOV', &
         'F_SPKCPV', &
         'F_SPKE15', &
         'F_SPKEZ', &
         'F_SPKEZP', &
         'F_SPKF15', &
         'F_SPKGEO', &
         'F_SPKGPS', &
         'F_SPKGPX', &
         'F_SPKGXC', &
         'F_SPKPDS', &
         'F_SPKPVN', &
         'F_SPKS19', &
         'F_SPKSPV', &
         'F_SPKW01', &
         'F_SRFNRM', &
         'F_SRFTRN', &
         'F_SRFXPT', &
         'F_ST00', &
         'F_STLABX', &
         'F_STMP03', &
         'F_STPOOL', &
         'F_STR2ET', &
         'F_STRING', &
         'F_SUBPNT', &
         'F_SUBSLR', &
         'F_SURFNM', &
         'F_SURFPV', &
         'F_SWAPAC', &
         'F_SWAPAD', &
         'F_SWAPAI', &
         'F_SXFORM', &
         'F_SYMTBC', &
         'F_SYMTBD', &
         'F_SYMTBI', &
         'F_T_URAND', &
         'F_TABTXT', &
         'F_TANGPT0', &
         'F_TANGPT1', &
         'F_TANGPT2', &
         'F_TANGPT3', &
         'F_TANGPTW', &
         'F_TCHECK', &
         'F_TERM', &
         'F_TERMPT', &
         'F_TEXPYR', &
         'F_TIMCVR', &
         'F_TIMDEF', &
         'F_TIMOUT', &
         'F_TKFRAM', &
         'F_TLE', &
         'F_TNCNSG', &
         'F_TPARSE', &
         'F_TPARTV1', &
         'F_TPARTV2', &
         'F_TRGSEP', &
         'F_TSTCK3', &
         'F_TTRANS', &
         'F_TWOVXF', &
         'F_UTC2ET', &
         'F_VECTOR3', &
         'F_VECTORG', &
         'F_VOXEL', &
         'F_VSTRNG', &
         'F_WIN', &
         'F_XDDA', &
         'F_XFMSTA', &
         'F_XFNEUL', &
         'F_XFRAV', &
         'F_XLATED', &
         'F_XLATEI', &
         'F_ZZASC1', &
         'F_ZZASC2', &
         'F_ZZASRYEL', &
         'F_ZZBDIN', &
         'F_ZZBDKR', &
         'F_ZZBDTN', &
         'F_ZZBDTRN', &
         'F_ZZBODS', &
         'F_ZZBODS2C', &
         'F_ZZBODVCD', &
         'F_ZZBQUAD', &
         'F_ZZCKE06', &
         'F_ZZCNQUAD', &
         'F_ZZCORSXF', &
         'F_ZZCTR', &
         'F_ZZCXBRUT', &
         'F_ZZDASGRC', &
         'F_ZZDDHNFC', &
         'F_ZZDGDR', &
         'F_ZZDGFR', &
         'F_ZZDGSR', &
         'F_ZZDM', &
         'F_ZZDSKBSR', &
         'F_ZZDSKBUX', &
         'F_ZZDSKSEL', &
         'F_ZZDSKSGR', &
         'F_ZZDSKSGX', &
         'F_ZZDSKSPH', &
         'F_ZZEDTMPT', &
         'F_ZZELLBDS', &
         'F_ZZELNAXX', &
         'F_ZZELVUPY0', &
         'F_ZZFDAT', &
         'F_ZZFGEO', &
         'F_ZZFOVAXI', &
         'F_ZZGETELM', &
         'F_ZZGFCOQ', &
         'F_ZZGFCOST', &
         'F_ZZGFCOU', &
         'F_ZZGFCPRX', &
         'F_ZZGFCSLV', &
         'F_ZZGFDIQ', &
         'F_ZZGFDIU', &
         'F_ZZGFFVU', &
         'F_ZZGFILU', &
         'F_ZZGFLNG1', &
         'F_ZZGFLNG2', &
         'F_ZZGFLNG3', &
         'F_ZZGFOCU', &
         'F_ZZGFPAU', &
         'F_ZZGFREL', &
         'F_ZZGFRELX', &
         'F_ZZGFRPWK', &
         'F_ZZGFRRU', &
         'F_ZZGFSOLV', &
         'F_ZZGFSOLVX', &
         'F_ZZGFSPQ', &
         'F_ZZGFSPU', &
         'F_ZZGFSSIN', &
         'F_ZZGFSSOB', &
         'F_ZZGFWSTS', &
         'F_ZZHSC', &
         'F_ZZHSI', &
         'F_ZZHULLAX', &
         'F_ZZILUSTA', &
         'F_ZZINLAT', &
         'F_ZZINLAT0', &
         'F_ZZINPDT', &
         'F_ZZINPDT0', &
         'F_ZZINREC', &
         'F_ZZINVELT', &
         'F_ZZLATBOX', &
         'F_ZZMKSPIN', &
         'F_ZZMSXF', &
         'F_ZZNAMFRM', &
         'F_ZZNOFCON', &
         'F_ZZNRMLON', &
         'F_ZZOCCED', &
         'F_ZZOCCED2', &
         'F_ZZOCCED3', &
         'F_ZZPDCMPL', &
         'F_ZZPDPLTC', &
         'F_ZZPDTBOX', &
         'F_ZZPLAT', &
         'F_ZZPTPL02', &
         'F_ZZRAYBOX', &
         'F_ZZRECBOX', &
         'F_ZZRTNMAT', &
         'F_ZZRXR', &
         'F_ZZRYTELT', &
         'F_ZZRYTLAT', &
         'F_ZZRYTPDT', &
         'F_ZZRYTREC', &
         'F_ZZSC01', &
         'F_ZZSEGBOX', &
         'F_ZZSEPQ', &
         'F_ZZSFXCOR', &
         'F_ZZSGLATX', &
         'F_ZZSINUTL', &
         'F_ZZSPKFUN', &
         'F_ZZSTELAB', &
         'F_ZZSWFFET', &
         'F_ZZSWFXFM', &
         'F_ZZTANSLV', &
         'F_ZZTANUTL', &
         'F_ZZTIME', &
         'F_ZZUTC', &
         'F_ZZVALCOR', &
         'F_ZZVRTPLT', &
         'F_ZZWIND2D' ]

      CHARACTER(LEN=1024)   :: CMLINE
      CHARACTER(LEN=NAMLEN) :: ARG
      LOGICAL               :: CHOSEN(NFAM)
      LOGICAL               :: ANYSEL
      LOGICAL               :: FAILED
      LOGICAL               :: OK
      INTEGER               :: I
      INTEGER               :: J
      INTEGER               :: LENGTH

!
!     Parse the arguments: options start with '-' (those other than
!     --list are handled by TSETUP); everything else is a family name.
!
      CHOSEN = .FALSE.
      ANYSEL = .FALSE.

      DO I = 1, COMMAND_ARGUMENT_COUNT()

         CALL GET_COMMAND_ARGUMENT ( I, ARG, LENGTH )
         ARG = UPPER ( ARG )

         IF ( ARG == '-L' .OR. ARG == '--LIST' ) THEN
            DO J = 1, NFAM
               WRITE (*, '(A)') TRIM ( FAMILY(J) )
            END DO
            STOP
         ELSE IF ( ARG(1:1) == '-' ) THEN
            CYCLE
         END IF

         J = FINDLOC ( FAMILY, ARG, DIM=1 )

         IF ( LENGTH > NAMLEN .OR. J == 0 ) THEN
            CALL GET_COMMAND_ARGUMENT ( I, CMLINE )
            WRITE (*, '(A)') 'tspice: unknown test family: ' // TRIM ( CMLINE )
            WRITE (*, '(A)') 'Use --list to show the available families.'
            STOP 2
         END IF

         CHOSEN(J) = .TRUE.
         ANYSEL    = .TRUE.

      END DO

      IF ( .NOT. ANYSEL ) CHOSEN = .TRUE.

      CALL GETCML ( CMLINE )
      CALL TSETUP ( CMLINE, 'spice{0-9}{0-9}.log', '1.0.0' )

      DO I = 1, NFAM
         IF ( CHOSEN(I) ) CALL RUN_FAMILY ( FAMILY(I), OK )
      END DO

      CALL TCLOSE

!
!     Exit status: failure if any test case in any family failed.
!
      CALL T_ANYBAD ( FAILED )

      IF ( FAILED ) THEN
         CALL BYEBYE ( 'FAILURE' )
      ELSE
         CALL BYEBYE ( 'SUCCESS' )
      END IF

      CONTAINS

      PURE FUNCTION UPPER ( S ) RESULT ( U )
         CHARACTER(LEN=*), INTENT(IN) :: S
         CHARACTER(LEN=LEN(S))        :: U
         INTEGER                      :: K
         U = S
         DO K = 1, LEN(S)
            IF ( 'a' <= S(K:K) .AND. S(K:K) <= 'z' ) THEN
               U(K:K) = ACHAR ( IACHAR ( S(K:K) ) - 32 )
            END IF
         END DO
      END FUNCTION UPPER

      SUBROUTINE RUN_FAMILY ( NAME, OK )
         CHARACTER(LEN=*), INTENT(IN)  :: NAME
         LOGICAL,          INTENT(OUT) :: OK

         SELECT CASE ( NAME )
         CASE ( 'F_AAAAPHSH' )
            CALL F_AAAAPHSH ( OK )
         CASE ( 'F_AB' )
            CALL F_AB ( OK )
         CASE ( 'F_ASNACSN' )
            CALL F_ASNACSN ( OK )
         CASE ( 'F_AZL' )
            CALL F_AZL ( OK )
         CASE ( 'F_AZLCPV' )
            CALL F_AZLCPV ( OK )
         CASE ( 'F_BADKPV' )
            CALL F_BADKPV ( OK )
         CASE ( 'F_BODCOD' )
            CALL F_BODCOD ( OK )
         CASE ( 'F_BODVAR' )
            CALL F_BODVAR ( OK )
         CASE ( 'F_CCIFRM' )
            CALL F_CCIFRM ( OK )
         CASE ( 'F_CHBIGR' )
            CALL F_CHBIGR ( OK )
         CASE ( 'F_CHBSHV' )
            CALL F_CHBSHV ( OK )
         CASE ( 'F_CHGIRF' )
            CALL F_CHGIRF ( OK )
         CASE ( 'F_CHGRFX' )
            CALL F_CHGRFX ( OK )
         CASE ( 'F_CK02' )
            CALL F_CK02 ( OK )
         CASE ( 'F_CK03' )
            CALL F_CK03 ( OK )
         CASE ( 'F_CK05' )
            CALL F_CK05 ( OK )
         CASE ( 'F_CK06' )
            CALL F_CK06 ( OK )
         CASE ( 'F_CKBSR' )
            CALL F_CKBSR ( OK )
         CASE ( 'F_CKCOV' )
            CALL F_CKCOV ( OK )
         CASE ( 'F_CKFROT' )
            CALL F_CKFROT ( OK )
         CASE ( 'F_CKFXFM' )
            CALL F_CKFXFM ( OK )
         CASE ( 'F_CKGP' )
            CALL F_CKGP ( OK )
         CASE ( 'F_CKMETA' )
            CALL F_CKMETA ( OK )
         CASE ( 'F_CKW01' )
            CALL F_CKW01 ( OK )
         CASE ( 'F_CKW02' )
            CALL F_CKW02 ( OK )
         CASE ( 'F_CKW03' )
            CALL F_CKW03 ( OK )
         CASE ( 'F_CONVRT' )
            CALL F_CONVRT ( OK )
         CASE ( 'F_CRDCNV' )
            CALL F_CRDCNV ( OK )
         CASE ( 'F_CYIP' )
            CALL F_CYIP ( OK )
         CASE ( 'F_DAFAH' )
            CALL F_DAFAH ( OK )
         CASE ( 'F_DAFANA' )
            CALL F_DAFANA ( OK )
         CASE ( 'F_DAFNN' )
            CALL F_DAFNN ( OK )
         CASE ( 'F_DAS' )
            CALL F_DAS ( OK )
         CASE ( 'F_DASA2L' )
            CALL F_DASA2L ( OK )
         CASE ( 'F_DASCUD' )
            CALL F_DASCUD ( OK )
         CASE ( 'F_DASFR' )
            CALL F_DASFR ( OK )
         CASE ( 'F_DASMUL' )
            CALL F_DASMUL ( OK )
         CASE ( 'F_DDHCLS' )
            CALL F_DDHCLS ( OK )
         CASE ( 'F_DDHCLU' )
            CALL F_DDHCLU ( OK )
         CASE ( 'F_DDHF2H' )
            CALL F_DDHF2H ( OK )
         CASE ( 'F_DDHFNH' )
            CALL F_DDHFNH ( OK )
         CASE ( 'F_DDHFTSIZE' )
            CALL F_DDHFTSIZE ( OK )
         CASE ( 'F_DDHGSD' )
            CALL F_DDHGSD ( OK )
         CASE ( 'F_DDHGTU' )
            CALL F_DDHGTU ( OK )
         CASE ( 'F_DDHHLU' )
            CALL F_DDHHLU ( OK )
         CASE ( 'F_DDHISN' )
            CALL F_DDHISN ( OK )
         CASE ( 'F_DDHIVF' )
            CALL F_DDHIVF ( OK )
         CASE ( 'F_DDHLUH' )
            CALL F_DDHLUH ( OK )
         CASE ( 'F_DDHNFO' )
            CALL F_DDHNFO ( OK )
         CASE ( 'F_DDHOPN' )
            CALL F_DDHOPN ( OK )
         CASE ( 'F_DDHPPF' )
            CALL F_DDHPPF ( OK )
         CASE ( 'F_DDHRCM' )
            CALL F_DDHRCM ( OK )
         CASE ( 'F_DDHRMU' )
            CALL F_DDHRMU ( OK )
         CASE ( 'F_DDHUNL' )
            CALL F_DDHUNL ( OK )
         CASE ( 'F_DHFA' )
            CALL F_DHFA ( OK )
         CASE ( 'F_DLA' )
            CALL F_DLA ( OK )
         CASE ( 'F_DNEARP' )
            CALL F_DNEARP ( OK )
         CASE ( 'F_DPFMT' )
            CALL F_DPFMT ( OK )
         CASE ( 'F_DPSTRF' )
            CALL F_DPSTRF ( OK )
         CASE ( 'F_DSK02' )
            CALL F_DSK02 ( OK )
         CASE ( 'F_DSKKPR' )
            CALL F_DSKKPR ( OK )
         CASE ( 'F_DSKMI2' )
            CALL F_DSKMI2 ( OK )
         CASE ( 'F_DSKOBJ' )
            CALL F_DSKOBJ ( OK )
         CASE ( 'F_DSKRB2' )
            CALL F_DSKRB2 ( OK )
         CASE ( 'F_DSKTOL' )
            CALL F_DSKTOL ( OK )
         CASE ( 'F_DSKW02' )
            CALL F_DSKW02 ( OK )
         CASE ( 'F_DSKX02' )
            CALL F_DSKX02 ( OK )
         CASE ( 'F_DSKXSI' )
            CALL F_DSKXSI ( OK )
         CASE ( 'F_DSKXV' )
            CALL F_DSKXV ( OK )
         CASE ( 'F_DVOPS' )
            CALL F_DVOPS ( OK )
         CASE ( 'F_DVSEP' )
            CALL F_DVSEP ( OK )
         CASE ( 'F_DWPOOL' )
            CALL F_DWPOOL ( OK )
         CASE ( 'F_DYN01' )
            CALL F_DYN01 ( OK )
         CASE ( 'F_DYN02' )
            CALL F_DYN02 ( OK )
         CASE ( 'F_DYN03' )
            CALL F_DYN03 ( OK )
         CASE ( 'F_DYN04' )
            CALL F_DYN04 ( OK )
         CASE ( 'F_DYN05' )
            CALL F_DYN05 ( OK )
         CASE ( 'F_DYN06' )
            CALL F_DYN06 ( OK )
         CASE ( 'F_DYN07' )
            CALL F_DYN07 ( OK )
         CASE ( 'F_DYN08' )
            CALL F_DYN08 ( OK )
         CASE ( 'F_EDNMPT' )
            CALL F_EDNMPT ( OK )
         CASE ( 'F_EDPNT' )
            CALL F_EDPNT ( OK )
         CASE ( 'F_EK01' )
            CALL F_EK01 ( OK )
         CASE ( 'F_EK02' )
            CALL F_EK02 ( OK )
         CASE ( 'F_EK03' )
            CALL F_EK03 ( OK )
         CASE ( 'F_EK04' )
            CALL F_EK04 ( OK )
         CASE ( 'F_EQNCPV' )
            CALL F_EQNCPV ( OK )
         CASE ( 'F_ET2LST' )
            CALL F_ET2LST ( OK )
         CASE ( 'F_ET2UTC' )
            CALL F_ET2UTC ( OK )
         CASE ( 'F_ETCAL' )
            CALL F_ETCAL ( OK )
         CASE ( 'F_EULER' )
            CALL F_EULER ( OK )
         CASE ( 'F_FNDCMP' )
            CALL F_FNDCMP ( OK )
         CASE ( 'F_FOVRAY' )
            CALL F_FOVRAY ( OK )
         CASE ( 'F_FOVTRG' )
            CALL F_FOVTRG ( OK )
         CASE ( 'F_FRAMEX' )
            CALL F_FRAMEX ( OK )
         CASE ( 'F_FRFTCH' )
            CALL F_FRFTCH ( OK )
         CASE ( 'F_FRMCHG' )
            CALL F_FRMCHG ( OK )
         CASE ( 'F_FRMGET' )
            CALL F_FRMGET ( OK )
         CASE ( 'F_GE01' )
            CALL F_GE01 ( OK )
         CASE ( 'F_GETFOV' )
            CALL F_GETFOV ( OK )
         CASE ( 'F_GETFV2' )
            CALL F_GETFV2 ( OK )
         CASE ( 'F_GFBAIL' )
            CALL F_GFBAIL ( OK )
         CASE ( 'F_GFBIRP' )
            CALL F_GFBIRP ( OK )
         CASE ( 'F_GFDIRP' )
            CALL F_GFDIRP ( OK )
         CASE ( 'F_GFDIST' )
            CALL F_GFDIST ( OK )
         CASE ( 'F_GFEVNT' )
            CALL F_GFEVNT ( OK )
         CASE ( 'F_GFFOVE' )
            CALL F_GFFOVE ( OK )
         CASE ( 'F_GFILUM' )
            CALL F_GFILUM ( OK )
         CASE ( 'F_GFLORP' )
            CALL F_GFLORP ( OK )
         CASE ( 'F_GFOCCE' )
            CALL F_GFOCCE ( OK )
         CASE ( 'F_GFOCLT' )
            CALL F_GFOCLT ( OK )
         CASE ( 'F_GFPA' )
            CALL F_GFPA ( OK )
         CASE ( 'F_GFPCRP' )
            CALL F_GFPCRP ( OK )
         CASE ( 'F_GFPOSC' )
            CALL F_GFPOSC ( OK )
         CASE ( 'F_GFREFN' )
            CALL F_GFREFN ( OK )
         CASE ( 'F_GFRFOV' )
            CALL F_GFRFOV ( OK )
         CASE ( 'F_GFRPRT' )
            CALL F_GFRPRT ( OK )
         CASE ( 'F_GFRR' )
            CALL F_GFRR ( OK )
         CASE ( 'F_GFSCRP' )
            CALL F_GFSCRP ( OK )
         CASE ( 'F_GFSEP' )
            CALL F_GFSEP ( OK )
         CASE ( 'F_GFSNTC' )
            CALL F_GFSNTC ( OK )
         CASE ( 'F_GFSPRP' )
            CALL F_GFSPRP ( OK )
         CASE ( 'F_GFSTEP' )
            CALL F_GFSTEP ( OK )
         CASE ( 'F_GFSUBC' )
            CALL F_GFSUBC ( OK )
         CASE ( 'F_GFTFOV' )
            CALL F_GFTFOV ( OK )
         CASE ( 'F_GFUDB' )
            CALL F_GFUDB ( OK )
         CASE ( 'F_GFUDS' )
            CALL F_GFUDS ( OK )
         CASE ( 'F_GFXCRP' )
            CALL F_GFXCRP ( OK )
         CASE ( 'F_HRMITE' )
            CALL F_HRMITE ( OK )
         CASE ( 'F_ILLUMF' )
            CALL F_ILLUMF ( OK )
         CASE ( 'F_ILLUMG' )
            CALL F_ILLUMG ( OK )
         CASE ( 'F_ILUMIN' )
            CALL F_ILUMIN ( OK )
         CASE ( 'F_INCNSG' )
            CALL F_INCNSG ( OK )
         CASE ( 'F_INELPL' )
            CALL F_INELPL ( OK )
         CASE ( 'F_INRYPL' )
            CALL F_INRYPL ( OK )
         CASE ( 'F_INSANG' )
            CALL F_INSANG ( OK )
         CASE ( 'F_INSERT' )
            CALL F_INSERT ( OK )
         CASE ( 'F_IOVCMP' )
            CALL F_IOVCMP ( OK )
         CASE ( 'F_JUL2GR' )
            CALL F_JUL2GR ( OK )
         CASE ( 'F_KEEPER' )
            CALL F_KEEPER ( OK )
         CASE ( 'F_KPBUG' )
            CALL F_KPBUG ( OK )
         CASE ( 'F_KPSOLV' )
            CALL F_KPSOLV ( OK )
         CASE ( 'F_LAGRNG' )
            CALL F_LAGRNG ( OK )
         CASE ( 'F_LATSRF' )
            CALL F_LATSRF ( OK )
         CASE ( 'F_LIMBPT' )
            CALL F_LIMBPT ( OK )
         CASE ( 'F_LOCATI' )
            CALL F_LOCATI ( OK )
         CASE ( 'F_LPARSE' )
            CALL F_LPARSE ( OK )
         CASE ( 'F_LS' )
            CALL F_LS ( OK )
         CASE ( 'F_LTIME' )
            CALL F_LTIME ( OK )
         CASE ( 'F_M2Q' )
            CALL F_M2Q ( OK )
         CASE ( 'F_MATRIX3' )
            CALL F_MATRIX3 ( OK )
         CASE ( 'F_MATRIXG' )
            CALL F_MATRIXG ( OK )
         CASE ( 'F_MOVED' )
            CALL F_MOVED ( OK )
         CASE ( 'F_NEARPT' )
            CALL F_NEARPT ( OK )
         CASE ( 'F_NNEK01' )
            CALL F_NNEK01 ( OK )
         CASE ( 'F_NNEK03' )
            CALL F_NNEK03 ( OK )
         CASE ( 'F_NNEK04' )
            CALL F_NNEK04 ( OK )
         CASE ( 'F_NPEDLN' )
            CALL F_NPEDLN ( OK )
         CASE ( 'F_NPELPT' )
            CALL F_NPELPT ( OK )
         CASE ( 'F_NPLNPT' )
            CALL F_NPLNPT ( OK )
         CASE ( 'F_NPSGPT' )
            CALL F_NPSGPT ( OK )
         CASE ( 'F_OCCULT' )
            CALL F_OCCULT ( OK )
         CASE ( 'F_OSCELT' )
            CALL F_OSCELT ( OK )
         CASE ( 'F_OSCLTX' )
            CALL F_OSCLTX ( OK )
         CASE ( 'F_PCK20' )
            CALL F_PCK20 ( OK )
         CASE ( 'F_PCKBSR' )
            CALL F_PCKBSR ( OK )
         CASE ( 'F_PCKBUF' )
            CALL F_PCKBUF ( OK )
         CASE ( 'F_PCKCOV' )
            CALL F_PCKCOV ( OK )
         CASE ( 'F_PGR' )
            CALL F_PGR ( OK )
         CASE ( 'F_PHASEQ' )
            CALL F_PHASEQ ( OK )
         CASE ( 'F_PLN' )
            CALL F_PLN ( OK )
         CASE ( 'F_PLT' )
            CALL F_PLT ( OK )
         CASE ( 'F_PLTNP' )
            CALL F_PLTNP ( OK )
         CASE ( 'F_POLYDS' )
            CALL F_POLYDS ( OK )
         CASE ( 'F_POOL' )
            CALL F_POOL ( OK )
         CASE ( 'F_PRDFRM' )
            CALL F_PRDFRM ( OK )
         CASE ( 'F_PRJP' )
            CALL F_PRJP ( OK )
         CASE ( 'F_PXFORM' )
            CALL F_PXFORM ( OK )
         CASE ( 'F_PXFRM2' )
            CALL F_PXFRM2 ( OK )
         CASE ( 'F_Q2M' )
            CALL F_Q2M ( OK )
         CASE ( 'F_QDERIV' )
            CALL F_QDERIV ( OK )
         CASE ( 'F_QUAT' )
            CALL F_QUAT ( OK )
         CASE ( 'F_RC2GRD' )
            CALL F_RC2GRD ( OK )
         CASE ( 'F_RDPCK' )
            CALL F_RDPCK ( OK )
         CASE ( 'F_RDPCKD' )
            CALL F_RDPCKD ( OK )
         CASE ( 'F_REFCHG' )
            CALL F_REFCHG ( OK )
         CASE ( 'F_REGLON' )
            CALL F_REGLON ( OK )
         CASE ( 'F_REPMX' )
            CALL F_REPMX ( OK )
         CASE ( 'F_ROTGET' )
            CALL F_ROTGET ( OK )
         CASE ( 'F_SAELGV' )
            CALL F_SAELGV ( OK )
         CASE ( 'F_SBF' )
            CALL F_SBF ( OK )
         CASE ( 'F_SCLK' )
            CALL F_SCLK ( OK )
         CASE ( 'F_SCLK0' )
            CALL F_SCLK0 ( OK )
         CASE ( 'F_SCLK1' )
            CALL F_SCLK1 ( OK )
         CASE ( 'F_SCTRAN' )
            CALL F_SCTRAN ( OK )
         CASE ( 'F_SEPOOL' )
            CALL F_SEPOOL ( OK )
         CASE ( 'F_SGMETA' )
            CALL F_SGMETA ( OK )
         CASE ( 'F_SHARPR' )
            CALL F_SHARPR ( OK )
         CASE ( 'F_SINCPT' )
            CALL F_SINCPT ( OK )
         CASE ( 'F_SLICE' )
            CALL F_SLICE ( OK )
         CASE ( 'F_SPK01' )
            CALL F_SPK01 ( OK )
         CASE ( 'F_SPK02' )
            CALL F_SPK02 ( OK )
         CASE ( 'F_SPK03' )
            CALL F_SPK03 ( OK )
         CASE ( 'F_SPK05' )
            CALL F_SPK05 ( OK )
         CASE ( 'F_SPK08' )
            CALL F_SPK08 ( OK )
         CASE ( 'F_SPK09' )
            CALL F_SPK09 ( OK )
         CASE ( 'F_SPK10' )
            CALL F_SPK10 ( OK )
         CASE ( 'F_SPK12' )
            CALL F_SPK12 ( OK )
         CASE ( 'F_SPK13' )
            CALL F_SPK13 ( OK )
         CASE ( 'F_SPK14' )
            CALL F_SPK14 ( OK )
         CASE ( 'F_SPK17' )
            CALL F_SPK17 ( OK )
         CASE ( 'F_SPK18' )
            CALL F_SPK18 ( OK )
         CASE ( 'F_SPK19' )
            CALL F_SPK19 ( OK )
         CASE ( 'F_SPK20' )
            CALL F_SPK20 ( OK )
         CASE ( 'F_SPK21' )
            CALL F_SPK21 ( OK )
         CASE ( 'F_SPKAPO' )
            CALL F_SPKAPO ( OK )
         CASE ( 'F_SPKAPP' )
            CALL F_SPKAPP ( OK )
         CASE ( 'F_SPKBSR' )
            CALL F_SPKBSR ( OK )
         CASE ( 'F_SPKCOR' )
            CALL F_SPKCOR ( OK )
         CASE ( 'F_SPKCOV' )
            CALL F_SPKCOV ( OK )
         CASE ( 'F_SPKCPV' )
            CALL F_SPKCPV ( OK )
         CASE ( 'F_SPKE15' )
            CALL F_SPKE15 ( OK )
         CASE ( 'F_SPKEZ' )
            CALL F_SPKEZ ( OK )
         CASE ( 'F_SPKEZP' )
            CALL F_SPKEZP ( OK )
         CASE ( 'F_SPKF15' )
            CALL F_SPKF15 ( OK )
         CASE ( 'F_SPKGEO' )
            CALL F_SPKGEO ( OK )
         CASE ( 'F_SPKGPS' )
            CALL F_SPKGPS ( OK )
         CASE ( 'F_SPKGPX' )
            CALL F_SPKGPX ( OK )
         CASE ( 'F_SPKGXC' )
            CALL F_SPKGXC ( OK )
         CASE ( 'F_SPKPDS' )
            CALL F_SPKPDS ( OK )
         CASE ( 'F_SPKPVN' )
            CALL F_SPKPVN ( OK )
         CASE ( 'F_SPKS19' )
            CALL F_SPKS19 ( OK )
         CASE ( 'F_SPKSPV' )
            CALL F_SPKSPV ( OK )
         CASE ( 'F_SPKW01' )
            CALL F_SPKW01 ( OK )
         CASE ( 'F_SRFNRM' )
            CALL F_SRFNRM ( OK )
         CASE ( 'F_SRFTRN' )
            CALL F_SRFTRN ( OK )
         CASE ( 'F_SRFXPT' )
            CALL F_SRFXPT ( OK )
         CASE ( 'F_ST00' )
            CALL F_ST00 ( OK )
         CASE ( 'F_STLABX' )
            CALL F_STLABX ( OK )
         CASE ( 'F_STMP03' )
            CALL F_STMP03 ( OK )
         CASE ( 'F_STPOOL' )
            CALL F_STPOOL ( OK )
         CASE ( 'F_STR2ET' )
            CALL F_STR2ET ( OK )
         CASE ( 'F_STRING' )
            CALL F_STRING ( OK )
         CASE ( 'F_SUBPNT' )
            CALL F_SUBPNT ( OK )
         CASE ( 'F_SUBSLR' )
            CALL F_SUBSLR ( OK )
         CASE ( 'F_SURFNM' )
            CALL F_SURFNM ( OK )
         CASE ( 'F_SURFPV' )
            CALL F_SURFPV ( OK )
         CASE ( 'F_SWAPAC' )
            CALL F_SWAPAC ( OK )
         CASE ( 'F_SWAPAD' )
            CALL F_SWAPAD ( OK )
         CASE ( 'F_SWAPAI' )
            CALL F_SWAPAI ( OK )
         CASE ( 'F_SXFORM' )
            CALL F_SXFORM ( OK )
         CASE ( 'F_SYMTBC' )
            CALL F_SYMTBC ( OK )
         CASE ( 'F_SYMTBD' )
            CALL F_SYMTBD ( OK )
         CASE ( 'F_SYMTBI' )
            CALL F_SYMTBI ( OK )
         CASE ( 'F_T_URAND' )
            CALL F_T_URAND ( OK )
         CASE ( 'F_TABTXT' )
            CALL F_TABTXT ( OK )
         CASE ( 'F_TANGPT0' )
            CALL F_TANGPT0 ( OK )
         CASE ( 'F_TANGPT1' )
            CALL F_TANGPT1 ( OK )
         CASE ( 'F_TANGPT2' )
            CALL F_TANGPT2 ( OK )
         CASE ( 'F_TANGPT3' )
            CALL F_TANGPT3 ( OK )
         CASE ( 'F_TANGPTW' )
            CALL F_TANGPTW ( OK )
         CASE ( 'F_TCHECK' )
            CALL F_TCHECK ( OK )
         CASE ( 'F_TERM' )
            CALL F_TERM ( OK )
         CASE ( 'F_TERMPT' )
            CALL F_TERMPT ( OK )
         CASE ( 'F_TEXPYR' )
            CALL F_TEXPYR ( OK )
         CASE ( 'F_TIMCVR' )
            CALL F_TIMCVR ( OK )
         CASE ( 'F_TIMDEF' )
            CALL F_TIMDEF ( OK )
         CASE ( 'F_TIMOUT' )
            CALL F_TIMOUT ( OK )
         CASE ( 'F_TKFRAM' )
            CALL F_TKFRAM ( OK )
         CASE ( 'F_TLE' )
            CALL F_TLE ( OK )
         CASE ( 'F_TNCNSG' )
            CALL F_TNCNSG ( OK )
         CASE ( 'F_TPARSE' )
            CALL F_TPARSE ( OK )
         CASE ( 'F_TPARTV1' )
            CALL F_TPARTV1 ( OK )
         CASE ( 'F_TPARTV2' )
            CALL F_TPARTV2 ( OK )
         CASE ( 'F_TRGSEP' )
            CALL F_TRGSEP ( OK )
         CASE ( 'F_TSTCK3' )
            CALL F_TSTCK3 ( OK )
         CASE ( 'F_TTRANS' )
            CALL F_TTRANS ( OK )
         CASE ( 'F_TWOVXF' )
            CALL F_TWOVXF ( OK )
         CASE ( 'F_UTC2ET' )
            CALL F_UTC2ET ( OK )
         CASE ( 'F_VECTOR3' )
            CALL F_VECTOR3 ( OK )
         CASE ( 'F_VECTORG' )
            CALL F_VECTORG ( OK )
         CASE ( 'F_VOXEL' )
            CALL F_VOXEL ( OK )
         CASE ( 'F_VSTRNG' )
            CALL F_VSTRNG ( OK )
         CASE ( 'F_WIN' )
            CALL F_WIN ( OK )
         CASE ( 'F_XDDA' )
            CALL F_XDDA ( OK )
         CASE ( 'F_XFMSTA' )
            CALL F_XFMSTA ( OK )
         CASE ( 'F_XFNEUL' )
            CALL F_XFNEUL ( OK )
         CASE ( 'F_XFRAV' )
            CALL F_XFRAV ( OK )
         CASE ( 'F_XLATED' )
            CALL F_XLATED ( OK )
         CASE ( 'F_XLATEI' )
            CALL F_XLATEI ( OK )
         CASE ( 'F_ZZASC1' )
            CALL F_ZZASC1 ( OK )
         CASE ( 'F_ZZASC2' )
            CALL F_ZZASC2 ( OK )
         CASE ( 'F_ZZASRYEL' )
            CALL F_ZZASRYEL ( OK )
         CASE ( 'F_ZZBDIN' )
            CALL F_ZZBDIN ( OK )
         CASE ( 'F_ZZBDKR' )
            CALL F_ZZBDKR ( OK )
         CASE ( 'F_ZZBDTN' )
            CALL F_ZZBDTN ( OK )
         CASE ( 'F_ZZBDTRN' )
            CALL F_ZZBDTRN ( OK )
         CASE ( 'F_ZZBODS' )
            CALL F_ZZBODS ( OK )
         CASE ( 'F_ZZBODS2C' )
            CALL F_ZZBODS2C ( OK )
         CASE ( 'F_ZZBODVCD' )
            CALL F_ZZBODVCD ( OK )
         CASE ( 'F_ZZBQUAD' )
            CALL F_ZZBQUAD ( OK )
         CASE ( 'F_ZZCKE06' )
            CALL F_ZZCKE06 ( OK )
         CASE ( 'F_ZZCNQUAD' )
            CALL F_ZZCNQUAD ( OK )
         CASE ( 'F_ZZCORSXF' )
            CALL F_ZZCORSXF ( OK )
         CASE ( 'F_ZZCTR' )
            CALL F_ZZCTR ( OK )
         CASE ( 'F_ZZCXBRUT' )
            CALL F_ZZCXBRUT ( OK )
         CASE ( 'F_ZZDASGRC' )
            CALL F_ZZDASGRC ( OK )
         CASE ( 'F_ZZDDHNFC' )
            CALL F_ZZDDHNFC ( OK )
         CASE ( 'F_ZZDGDR' )
            CALL F_ZZDGDR ( OK )
         CASE ( 'F_ZZDGFR' )
            CALL F_ZZDGFR ( OK )
         CASE ( 'F_ZZDGSR' )
            CALL F_ZZDGSR ( OK )
         CASE ( 'F_ZZDM' )
            CALL F_ZZDM ( OK )
         CASE ( 'F_ZZDSKBSR' )
            CALL F_ZZDSKBSR ( OK )
         CASE ( 'F_ZZDSKBUX' )
            CALL F_ZZDSKBUX ( OK )
         CASE ( 'F_ZZDSKSEL' )
            CALL F_ZZDSKSEL ( OK )
         CASE ( 'F_ZZDSKSGR' )
            CALL F_ZZDSKSGR ( OK )
         CASE ( 'F_ZZDSKSGX' )
            CALL F_ZZDSKSGX ( OK )
         CASE ( 'F_ZZDSKSPH' )
            CALL F_ZZDSKSPH ( OK )
         CASE ( 'F_ZZEDTMPT' )
            CALL F_ZZEDTMPT ( OK )
         CASE ( 'F_ZZELLBDS' )
            CALL F_ZZELLBDS ( OK )
         CASE ( 'F_ZZELNAXX' )
            CALL F_ZZELNAXX ( OK )
         CASE ( 'F_ZZELVUPY0' )
            CALL F_ZZELVUPY0 ( OK )
         CASE ( 'F_ZZFDAT' )
            CALL F_ZZFDAT ( OK )
         CASE ( 'F_ZZFGEO' )
            CALL F_ZZFGEO ( OK )
         CASE ( 'F_ZZFOVAXI' )
            CALL F_ZZFOVAXI ( OK )
         CASE ( 'F_ZZGETELM' )
            CALL F_ZZGETELM ( OK )
         CASE ( 'F_ZZGFCOQ' )
            CALL F_ZZGFCOQ ( OK )
         CASE ( 'F_ZZGFCOST' )
            CALL F_ZZGFCOST ( OK )
         CASE ( 'F_ZZGFCOU' )
            CALL F_ZZGFCOU ( OK )
         CASE ( 'F_ZZGFCPRX' )
            CALL F_ZZGFCPRX ( OK )
         CASE ( 'F_ZZGFCSLV' )
            CALL F_ZZGFCSLV ( OK )
         CASE ( 'F_ZZGFDIQ' )
            CALL F_ZZGFDIQ ( OK )
         CASE ( 'F_ZZGFDIU' )
            CALL F_ZZGFDIU ( OK )
         CASE ( 'F_ZZGFFVU' )
            CALL F_ZZGFFVU ( OK )
         CASE ( 'F_ZZGFILU' )
            CALL F_ZZGFILU ( OK )
         CASE ( 'F_ZZGFLNG1' )
            CALL F_ZZGFLNG1 ( OK )
         CASE ( 'F_ZZGFLNG2' )
            CALL F_ZZGFLNG2 ( OK )
         CASE ( 'F_ZZGFLNG3' )
            CALL F_ZZGFLNG3 ( OK )
         CASE ( 'F_ZZGFOCU' )
            CALL F_ZZGFOCU ( OK )
         CASE ( 'F_ZZGFPAU' )
            CALL F_ZZGFPAU ( OK )
         CASE ( 'F_ZZGFREL' )
            CALL F_ZZGFREL ( OK )
         CASE ( 'F_ZZGFRELX' )
            CALL F_ZZGFRELX ( OK )
         CASE ( 'F_ZZGFRPWK' )
            CALL F_ZZGFRPWK ( OK )
         CASE ( 'F_ZZGFRRU' )
            CALL F_ZZGFRRU ( OK )
         CASE ( 'F_ZZGFSOLV' )
            CALL F_ZZGFSOLV ( OK )
         CASE ( 'F_ZZGFSOLVX' )
            CALL F_ZZGFSOLVX ( OK )
         CASE ( 'F_ZZGFSPQ' )
            CALL F_ZZGFSPQ ( OK )
         CASE ( 'F_ZZGFSPU' )
            CALL F_ZZGFSPU ( OK )
         CASE ( 'F_ZZGFSSIN' )
            CALL F_ZZGFSSIN ( OK )
         CASE ( 'F_ZZGFSSOB' )
            CALL F_ZZGFSSOB ( OK )
         CASE ( 'F_ZZGFWSTS' )
            CALL F_ZZGFWSTS ( OK )
         CASE ( 'F_ZZHSC' )
            CALL F_ZZHSC ( OK )
         CASE ( 'F_ZZHSI' )
            CALL F_ZZHSI ( OK )
         CASE ( 'F_ZZHULLAX' )
            CALL F_ZZHULLAX ( OK )
         CASE ( 'F_ZZILUSTA' )
            CALL F_ZZILUSTA ( OK )
         CASE ( 'F_ZZINLAT' )
            CALL F_ZZINLAT ( OK )
         CASE ( 'F_ZZINLAT0' )
            CALL F_ZZINLAT0 ( OK )
         CASE ( 'F_ZZINPDT' )
            CALL F_ZZINPDT ( OK )
         CASE ( 'F_ZZINPDT0' )
            CALL F_ZZINPDT0 ( OK )
         CASE ( 'F_ZZINREC' )
            CALL F_ZZINREC ( OK )
         CASE ( 'F_ZZINVELT' )
            CALL F_ZZINVELT ( OK )
         CASE ( 'F_ZZLATBOX' )
            CALL F_ZZLATBOX ( OK )
         CASE ( 'F_ZZMKSPIN' )
            CALL F_ZZMKSPIN ( OK )
         CASE ( 'F_ZZMSXF' )
            CALL F_ZZMSXF ( OK )
         CASE ( 'F_ZZNAMFRM' )
            CALL F_ZZNAMFRM ( OK )
         CASE ( 'F_ZZNOFCON' )
            CALL F_ZZNOFCON ( OK )
         CASE ( 'F_ZZNRMLON' )
            CALL F_ZZNRMLON ( OK )
         CASE ( 'F_ZZOCCED' )
            CALL F_ZZOCCED ( OK )
         CASE ( 'F_ZZOCCED2' )
            CALL F_ZZOCCED2 ( OK )
         CASE ( 'F_ZZOCCED3' )
            CALL F_ZZOCCED3 ( OK )
         CASE ( 'F_ZZPDCMPL' )
            CALL F_ZZPDCMPL ( OK )
         CASE ( 'F_ZZPDPLTC' )
            CALL F_ZZPDPLTC ( OK )
         CASE ( 'F_ZZPDTBOX' )
            CALL F_ZZPDTBOX ( OK )
         CASE ( 'F_ZZPLAT' )
            CALL F_ZZPLAT ( OK )
         CASE ( 'F_ZZPTPL02' )
            CALL F_ZZPTPL02 ( OK )
         CASE ( 'F_ZZRAYBOX' )
            CALL F_ZZRAYBOX ( OK )
         CASE ( 'F_ZZRECBOX' )
            CALL F_ZZRECBOX ( OK )
         CASE ( 'F_ZZRTNMAT' )
            CALL F_ZZRTNMAT ( OK )
         CASE ( 'F_ZZRXR' )
            CALL F_ZZRXR ( OK )
         CASE ( 'F_ZZRYTELT' )
            CALL F_ZZRYTELT ( OK )
         CASE ( 'F_ZZRYTLAT' )
            CALL F_ZZRYTLAT ( OK )
         CASE ( 'F_ZZRYTPDT' )
            CALL F_ZZRYTPDT ( OK )
         CASE ( 'F_ZZRYTREC' )
            CALL F_ZZRYTREC ( OK )
         CASE ( 'F_ZZSC01' )
            CALL F_ZZSC01 ( OK )
         CASE ( 'F_ZZSEGBOX' )
            CALL F_ZZSEGBOX ( OK )
         CASE ( 'F_ZZSEPQ' )
            CALL F_ZZSEPQ ( OK )
         CASE ( 'F_ZZSFXCOR' )
            CALL F_ZZSFXCOR ( OK )
         CASE ( 'F_ZZSGLATX' )
            CALL F_ZZSGLATX ( OK )
         CASE ( 'F_ZZSINUTL' )
            CALL F_ZZSINUTL ( OK )
         CASE ( 'F_ZZSPKFUN' )
            CALL F_ZZSPKFUN ( OK )
         CASE ( 'F_ZZSTELAB' )
            CALL F_ZZSTELAB ( OK )
         CASE ( 'F_ZZSWFFET' )
            CALL F_ZZSWFFET ( OK )
         CASE ( 'F_ZZSWFXFM' )
            CALL F_ZZSWFXFM ( OK )
         CASE ( 'F_ZZTANSLV' )
            CALL F_ZZTANSLV ( OK )
         CASE ( 'F_ZZTANUTL' )
            CALL F_ZZTANUTL ( OK )
         CASE ( 'F_ZZTIME' )
            CALL F_ZZTIME ( OK )
         CASE ( 'F_ZZUTC' )
            CALL F_ZZUTC ( OK )
         CASE ( 'F_ZZVALCOR' )
            CALL F_ZZVALCOR ( OK )
         CASE ( 'F_ZZVRTPLT' )
            CALL F_ZZVRTPLT ( OK )
         CASE ( 'F_ZZWIND2D' )
            CALL F_ZZWIND2D ( OK )
         END SELECT
      END SUBROUTINE RUN_FAMILY

      END PROGRAM TSPICE
