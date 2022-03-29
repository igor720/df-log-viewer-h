{-|
Module      : Symbols
Copyright   : (c) 2022 Igor Chudaev (mamont, igor720)
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : igor720@gmail.com
Stability   : experimental
Portability : non-portable

Log text symbols translation
-}
{-# LANGUAGE BinaryLiterals #-}

module Symbols where

import Data.Text ( Text )
import qualified Data.Text as T
import Data.Char ( ord, chr )
import Data.Maybe ( fromMaybe )
import qualified Data.IntMap.Strict as M


type SymMapping = M.IntMap Char

symMapping :: SymMapping
symMapping = M.fromList
    [ (160, chr 0x41)
    ]

textTranslate :: SymMapping -> Text -> Text
textTranslate sm =
    T.map (\ch -> fromMaybe ch (M.lookup (ord ch) sm))


{-
################################################################################
## Character table for DF game (not applicable here)
## (https://dwarffortresswiki.org/index.php/Character_table)
################################################################################
-- symMapping :: SymMapping
-- symMapping = M.fromList
--     [                    (1,   chr 0x263C), (2,   chr 0x263B), (3,   chr 0x2665)
--     , (4,   chr 0x2666), (5,   chr 0x2663), (6,   chr 0x2660), (7,   chr 0x2022)
--     , (8,   chr 0x25D8), (9,   chr 0x25CB), (10,  chr 0x25D9), (11,  chr 0x2642)
--     , (12,  chr 0x2640), (13,  chr 0x266A), (14,  chr 0x266B), (15,  chr 0x263C)

--     , (16,  chr 0x25BA), (17,  chr 0x25C4), (18,  chr 0x2195), (19,  chr 0x203C)
--     , (20,  chr 0x86),   (21,  chr 0xA7),   (22,  chr 0x25AC), (23,  chr 0x21A8)
--     , (24,  chr 0x2129), (25,  chr 0x2193), (26,  chr 0x2192), (27,  chr 0x2190)
--     , (28,  chr 0x221F), (29,  chr 0x2194), (30,  chr 0x25B2), (31,  chr 0x25BC)

--     , (127, chr 0x2302)

--     , (128, chr 0xC7),   (129, chr 0xFC),   (130, chr 0xE9),   (131, chr 0xE2)
--     , (132, chr 0xE4),   (133, chr 0xE0),   (134, chr 0xE5),   (135, chr 0xE7)
--     , (136, chr 0xEA),   (137, chr 0xEB),   (138, chr 0xE8),   (139, chr 0xEF)
--     , (140, chr 0xEE),   (141, chr 0xEC),   (142, chr 0xC4),   (143, chr 0xC5)

--     , (144, chr 0xC9),   (145, chr 0xE6),   (146, chr 0xC6),   (147, chr 0xF4)
--     , (148, chr 0xF6),   (149, chr 0xF2),   (150, chr 0xFB),   (151, chr 0xF9)
--     , (152, chr 0xFF),   (153, chr 0xD6),   (154, chr 0xDC),   (155, chr 0xA2)
--     , (156, chr 0xA3),   (157, chr 0xA5),   (158, chr 0x20A7), (159, chr 0x192)

--     , (160, chr 0xE1),   (161, chr 0xED),   (162, chr 0xF3),   (163, chr 0xFA)
--     , (164, chr 0xF1),   (165, chr 0xD1),   (166, chr 0xAA),   (167, chr 0xBA)
--     , (168, chr 0xBF),   (169, chr 0x2310), (170, chr 0xAC),   (171, chr 0xBD)
--     , (172, chr 0xBC),   (173, chr 0xA1),   (174, chr 0xAB),   (175, chr 0xBB)

--     , (176, chr 0x2591), (177, chr 0x2592), (178, chr 0x2593), (179, chr 0x2502)
--     , (180, chr 0x2524), (181, chr 0x2561), (182, chr 0x2562), (183, chr 0x2556)
--     , (184, chr 0x2555), (185, chr 0x2563), (186, chr 0x2551), (187, chr 0x2557)
--     , (188, chr 0x255D), (189, chr 0x255C), (190, chr 0x255B), (191, chr 0x2510)

--     , (192, chr 0x2514), (193, chr 0x2534), (194, chr 0x252C), (195, chr 0x251C)
--     , (196, chr 0x2500), (197, chr 0x253C), (198, chr 0x255E), (199, chr 0x255F)
--     , (200, chr 0x255A), (201, chr 0x2554), (202, chr 0x2569), (203, chr 0x2566)
--     , (204, chr 0x2560), (205, chr 0x2550), (206, chr 0x256C), (207, chr 0x2567)

--     , (208, chr 0x2568), (209, chr 0x2564), (210, chr 0x2565), (211, chr 0x2559)
--     , (212, chr 0x2558), (213, chr 0x2552), (214, chr 0x2553), (215, chr 0x256B)
--     , (216, chr 0x256A), (217, chr 0x2518), (218, chr 0x250C), (219, chr 0x2588)
--     , (220, chr 0x2584), (221, chr 0x258C), (222, chr 0x2590), (223, chr 0x2580)

--     , (224, chr 0x381),  (225, chr 0xDF),   (226, chr 0x393),  (227, chr 0x3C0)
--     , (228, chr 0x3A3),  (229, chr 0x3C3),  (230, chr 0xB5),   (231, chr 0x3C4)
--     , (232, chr 0x3A6),  (233, chr 0x398),  (234, chr 0x3A9),  (235, chr 0x3B4)
--     , (236, chr 0x221E), (237, chr 0x3C6),  (238, chr 0x3B5),  (239, chr 0x2229)

--     , (240, chr 0x2261), (241, chr 0xB1),   (242, chr 0x2265), (243, chr 0x2264)
--     , (244, chr 0x2320), (245, chr 0x2321), (246, chr 0xF7),   (247, chr 0x2248)
--     , (248, chr 0xB0),   (249, chr 0x2219), (250, chr 0xB7),   (251, chr 0x22A1)
--     , (252, chr 0x207F), (253, chr 0xB2),   (254, chr 0x25A0), (255, chr 0xA0)
--     ]
-}
