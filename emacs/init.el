
;; need this here to prevent it from being added automatically by Package.el
;; (package-initialize)

(progn
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize)

  (mapc (lambda (f) (load (concat "~/.emacs.d/mine/" f ".el")))
        '(
          "../edge/deadgrep"

          "settings"
          "packages"
          "keys"

          "base/magit"
          "base/org"

          "extra/ruby"
          "extra/python"
          "extra/ocaml"
          )))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ansi-term-color-vector
   [unspecified "#151718" "#CE4045" "#9FCA56" "#DCCD69" "#55B5DB" "#A074C4" "#55B5DB" "#D4D7D6"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("d6d8db64482e42874c508f0595d008977326f8df89d0de13ff45e05e255a3446" "5e3f78b6792b56e441716008042f699a6b46f271075f276534fe1f8ccadbb6fa" "c5538be3108ef0b3e9b8352887f797b5d3d9641d7dc9591c1523595745927326" "a8c64539203994b0ec6e7d00bf1427ae94840ef42465f3ebcf9d53bd62ec4d8a" "424f8ecece092b46076514708518d746848d59cbda88681b11f82cd3b968be67" "a97964443cbe00f3cd8f2d7b97589a275fa8b185ae38827fa3638a607f02d102" "0574ebcddc942ac17a864d1cf1093de5b4f1ff5b0fc68ad1cfe1aff28f732ba2" "57f95012730e3a03ebddb7f2925861ade87f53d5bbb255398357731a7b1ac0e0" "76dc63684249227d64634c8f62326f3d40cdc60039c2064174a7e7a7a88b1587" "1e9001d2f6ffb095eafd9514b4d5974b720b275143fbc89ea046495a99c940b0" "2a7beed4f24b15f77160118320123d699282cbf196e0089f113245d4b729ba5d" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "834cbeacb6837f3ddca4a1a7b19b1af3834f36a701e8b15b628cad3d85c970ff" "80ae3a89f1eca6fb94a525004f66b544e347c6f756aaafb728c7cdaef85ea1f5" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "08f5da7e1f5064a2917af94f0dab946adfb25665b25450168ded749ec78a1145" "cde05ed51346d6925d29311fb131511115ae7612764297077ca1b61371e6b047" "c30d153e623dfe32184857790a0cad243b979e8b1104e057c4a6ffe2210856f7" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "5d3e0746023fc5e246eb3e0e48c1ccb5ce0387fc4273896c6cf02ee349c2eba8" "f5ad3af69f2b6b7c547208b8708d4fa7928b5697ca0845633d1d67c2d145952a" "ab0950f92dc5e6b667276888cb0cdbc35fd1c16f667170a62c15bd3ed5ae5c5a" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "cd03a600a5f470994ba01dd3d1ff52d5809b59b4a37357fa94ca50a6f7f07473" "f64c9f8b4241b680b186f4620afb9c82fa2a76cf4498a7431f90db59bb1892eb" "603a9c7f3ca3253cb68584cb26c408afcf4e674d7db86badcfe649dd3c538656" "40bc0ac47a9bd5b8db7304f8ef628d71e2798135935eb450483db0dbbfff8b11" "bc40f613df8e0d8f31c5eb3380b61f587e1b5bc439212e03d4ea44b26b4f408a" "c4d3da0593914fff8fd2fbea89452f1a767893c578b219e352c763680d278762" "a56a6bf2ecb2ce4fa79ba636d0a5cf81ad9320a988ec4e55441a16d66b0c10e0" "1e67765ecb4e53df20a96fb708a8601f6d7c8f02edb09d16c838e465ebe7f51b" "f81a9aabc6a70441e4a742dfd6d10b2bae1088830dc7aba9c9922f4b1bd2ba50" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "b51c2dda65e8e7e66ab1b06bc10b59e61c153b0cf928f296efab5a7574779fb6" "18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "aae95fc700f9f7ff70efbc294fc7367376aa9456356ae36ec234751040ed9168" "0eea76fe89061a7f6da195f4a976c0b91150de987b942fac2dd10992aea33833" "70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "d79ece4768dfc4bab488475b85c2a8748dcdc3690e11a922f6be5e526a20b485" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "bff3bf3adad639ec87c15766758ba3ae5492722fb77932ab5bfd9f232e911c2f" "1b1e54d9e0b607010937d697556cd5ea66ec9c01e555bb7acea776471da59055" "a3132bd39a977ddde4c002f8bd0ef181414c3fbe9228e3643b999491192680ad" "3ff96689086ebc06f5f813a804f7114195b7c703ed2f19b51e10026723711e33" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" "a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "5436e5df71047d1fdd1079afa8341a442b1e26dd68b35b7d3c5ef8bd222057d1" "3c98d13ae2fc7aa59f05c494e8a15664ff5fe5db5256663a907272869c4130dd" "71182be392aa922f3c05e70087a40805ef2d969b4f8f965dfc0fc3c2f5df6168" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "9d4787fa4d9e06acb0e5c51499bff7ea827983b8bcc7d7545ca41db521bd9261" "4d80487632a0a5a72737a7fc690f1f30266668211b17ba836602a8da890c2118" "8b313e1793da427e90c034dbe74f3ad9092ac291846c0f855908c42a6bda1ff4" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "6065eaee1ef5aa6bbfc594b60fed3dba9e22cff9fb37559861babda07d50a634" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "5a21604c4b1f2df98e67cda2347b8f42dc9ce471a48164fcb8d3d52c3a0d10be" "55ed02951e7b458e4cd18837eefa1956884c9afd22bb514f697fd1d2d1abb3d3" "c5a886cc9044d8e6690a60f33db45506221aa0777a82ad1f7fe11a96d203fa44" "55d31108a7dc4a268a1432cd60a7558824223684afecefa6fae327212c40f8d3" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "e8a976fbc7710b60b069f27f5b2f1e216ec8d228fe5091f677717d6375d2669f" "d25d9b2b1e800a74fea4f6d174c4bd1b9c19a7617b22cc349245a36417c56ece" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "419637b7a8c9cb43f273980f0c9879c0cbadace6b38efac0281e031772c84eb2" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "c6faf8734bd04d303d9a272daef7a1c37a85597e2de3c006a4319ecf579821a0" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(erc-hl-nicks-mode t)
 '(erc-hl-nicks-skip-nicks (quote ("so" "So")))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#3f1a1a")
 '(fringe-mode 4 nil (fringe))
 '(gnus-logo-colors (quote ("#0d7b72" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (markdown-mode jinja2-mode ripgrep tuareg rbit zenburn-theme epl rainbow-mode dockerfile-mode yaml-mode web-mode elm-mode org haskell-mode badger-theme whole-line-or-region smartrep magit highlight-numbers helm-projectile expand-region)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face))
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(vc-annotate-background "#0E0E0E")
 '(vc-annotate-color-map
   (quote
    ((20 . "#616161")
     (40 . "#9D9D9D")
     (60 . "#9D9D9D")
     (80 . "#C2C2C2")
     (100 . "#C2C2C2")
     (120 . "#D9D9D9")
     (140 . "#D9D9D9")
     (160 . "#E8E8E8")
     (180 . "#E8E8E8")
     (200 . "#E8E8E8")
     (220 . "#F0F0F0")
     (240 . "#F0F0F0")
     (260 . "#F0F0F0")
     (280 . "#F6F6F6")
     (300 . "#F6F6F6")
     (320 . "#F6F6F6")
     (340 . "#F9F9F9")
     (360 . "#F9F9F9"))))
 '(vc-annotate-very-old-color "#D9D9D9")
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(caml-types-def-face ((t (:background "red" :foreground "brightblack"))) t)
 '(caml-types-expr-face ((t (:background "green" :foreground "brightblack"))) t)
 '(caml-types-occ-face ((t (:background "blue" :foreground "brightblack"))) t)
 '(caml-types-scope-face ((t (:background "cyan" :foreground "brightblack"))) t)
 '(caml-types-typed-face ((t (:background "magenta" :foreground "brightblack"))) t))
