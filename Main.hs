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

-- Paleta (R, G, B) com tons de verde com azul
purplePalette :: Int -> Int -> Int -> [(Int,Int,Int)]
purplePalette n ini vari = [(ini+i*vari, 0, 150+i*vari) | i <- [0..n] ]

-- Paleta (R, G, B) com tons de vermelho com azul
redPalette :: Int -> Int -> Int -> [(Int,Int,Int)]
redPalette n ini vari = [(ini+i*vari, 0, 0) | i <- [0..n] ]


-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]

-------------------------------------------------------------------------------
-- Geração de retângulos e circulos
-------------------------------------------------------------------------------

genRectsInDiagonal :: Int -> Float -> [Rect]
genRectsInDiagonal n tam = [(((tam/2), (m*(w+gap))), w+20*m, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 5

createCirc :: Float -> Float -> Int -> Int -> String
createCirc r raioc ncirc varY = 
  printf (unlines $ ["   " ++ svgCircle ((x,y),raioc) (svgStyle (last (take (z+1) (purplePalette ncirc 60 10)))) | z <- [0..(ncirc-1)], x <- [(r + 60) + (r * (cos((fromIntegral z)*2*pi/(fromIntegral ncirc))))], y <- [(r + fromIntegral varY) + (r * (sin((fromIntegral z)*2*pi/(fromIntegral ncirc))))]])
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
main :: IO ()
main = do
  writeFile "img.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ createCirc r raioc c desY ++ createCirc r raioc c2 desY2 ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInDiagonal nrects w
        palette = purplePalette nrects ini vari
        nrects = 24                                 --num de retangulos
        ini = 10                                    --inicio paleta de cor
        vari = 10                                   --variancia na paleta de cor
        r = 200                                     --raio total do circulo
        raioc = 40                                  --raio de cada circulo
        c = 20                                      --quantidade de circulos
        c2 = div c 2                                --quantidade de circulos
        desY = 40                                   --desloc em y do prim circ
        desY2 = 800                                 --desloc em y do seg circ
        (w,h) = (1400,1400)                         --tamanho da imagem
