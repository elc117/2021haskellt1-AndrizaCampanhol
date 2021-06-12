-- T1 de Haskell
-- Nome: Andriza Campanhol

import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)

-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta (R, G, B) com tons de verde com azul
bluePalette :: Int -> Int -> Int -> [(Int,Int,Int)]
bluePalette n ini vari = [(0, ini+i*vari, 100+i*vari) | i <- [0..n] ]


-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]

-------------------------------------------------------------------------------
-- Geração de retângulos e circulos
-------------------------------------------------------------------------------

genRectsInDiagonal :: Int -> [Rect]
genRectsInDiagonal n  = [((m*(w+gap), (m*(w+gap))), w+20*m, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 5

createCirc :: Float -> Int -> String
createCirc r ncirc = 
  printf (unlines $ ["   " ++ svgCircle ((x,y),20) (svgStyle (last (take (z+1) (bluePalette ncirc 80 10)))) | z <- [0..(ncirc-1)], x <- [(r + 50) + (r * (cos((fromIntegral z)*2*pi/(fromIntegral ncirc))))], y <- [(r + 50) + (r * (sin((fromIntegral z)*2*pi/(fromIntegral ncirc))))]])
-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

-- Gera string representando circulos SVG 
-- dadas coordenadas e raio do circulo e uma string com atributos de estilo
svgCircle :: Circle -> String -> String
svgCircle ((x,y),r) style =
  printf "<circle cx='%.3f' cy='%.3f' r='%.2f' style='%s'/>" x y r style

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------
img1 :: IO ()
img1 = do
  writeFile "img1.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInDiagonal nrects
        palette = bluePalette nrects ini variancia
        nrects = 20
        ini = 10
        variancia = 10
        (w,h) = (1500,1500) 

img2 :: IO()
img2 = do
  writeFile "img2.svg" $ svgstr
  where svgstr = svgBegin w h ++ createCirc r c ++ svgEnd
        (w, h) = (550, 550)
        r = 100
        c = 15

main :: IO()
main = do
  img1
  img2