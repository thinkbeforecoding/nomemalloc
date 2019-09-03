module Game

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input


type Window<'t, 's 
        when 't: struct
        and 't:> ValueType
        and 't: (new : unit -> 't)>(width, height, surfaceFormat, initialState, render, fps) as self =

    inherit Game()
    let graphics = new GraphicsDeviceManager(self, IsFullScreen = false, PreferredBackBufferWidth = width, PreferredBackBufferHeight = height)
    let mutable spriteBatch : SpriteBatch = null
    let mutable texture = null
    let mutable state: 's = initialState
    let data = Array.zeroCreate<'t> (width * height)
    let targetElapsedTime = TimeSpan.FromSeconds(1./double fps)
    

    override this.Initialize() =
        this.Content.RootDirectory <- "Content"
        this.Window.AllowAltF4 <- true
        this.IsMouseVisible <- false
        this.TargetElapsedTime <- targetElapsedTime
        
        base.Initialize()

    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        texture <- new Texture2D(this.GraphicsDevice, width, height, false, surfaceFormat)
        texture.SetData(data,0, width * height)

    override this.Update(gameTime: GameTime) =
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape)) then
            this.Exit()


        base.Update(gameTime)

    override this.Draw(gameTime: GameTime) =
        state <- render state gameTime (data.AsMemory())
        texture.SetData(data,0, width*height)

        spriteBatch.Begin()
        spriteBatch.Draw(texture,Rectangle(Point(0,0),Point(width, height)), Color(0xffffffffu))
        spriteBatch.End()

        base.Draw(gameTime)
