{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) 2014 Edward Kmett
-- License   :  BSD2
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This attempts to compile all of the shaders in the shaders/
-- directory as a basic sanity check.
--------------------------------------------------------------------
module Main where

import Control.Exception
import qualified Data.ByteString as BS
import Data.FileEmbed
import Data.Foldable
import Foreign.C.String
import Graphics.GL
import Prelude
import Quine.GL
import Quine.GL.Shader
import System.Exit
import System.FilePath
import System.IO

testShader :: GLenum -> FilePath -> IO ()
testShader st fp = do
  try (compile ["/shaders"] st ("shaders" </> fp)) >>= \case
    Right a -> putStrLn $ fp ++ " ok."
    Left (ShaderException _ log) -> do
      putStrLn $ fp ++ ": error in " ++ showShaderType 0 st ""
      BS.putStrLn log
      exitFailure

main :: IO ()
main = do
  putStr "Initializing SDL..."
  SDL.init SDL_INIT_EVERYTHING >>= err
  putStrLn "done"
  contextMajorVersion $= 4
  contextMinorVersion $= 1
  contextProfileMask $= SDL_GL_CONTEXT_PROFILE_CORE
  redSize $= 5
  greenSize $= 5
  blueSize $= 5
  depthSize $= 16
  doubleBuffer $= True
  putStr "Creating context..."
  window <- withCString "shaders" $ \windowName -> createWindow windowName SDL_WINDOWPOS_CENTERED SDL_WINDOWPOS_CENTERED 1 1 SDL_WINDOW_OPENGL
  cxt <- glCreateContext window
  putStrLn "done"
  makeCurrent window cxt
  ss <- getDir "shaders"
  buildNamedStrings ss ("/shaders"</>)
  putStrLn "Compiling Shaders:"
  for_ ss $ \(fp,_) -> case takeExtension fp of
    ".geom" -> testShader GL_GEOMETRY_SHADER fp
    ".frag" -> testShader GL_FRAGMENT_SHADER fp
    ".vert" -> testShader GL_VERTEX_SHADER fp
    e       -> return ()
  putStr "Cleaning up..."
  glDeleteContext cxt
  destroyWindow window
  quit
  putStrLn "done"
  hFlush stdout
  exitSuccess
