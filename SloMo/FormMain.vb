Imports System.Threading

Public Class FormMain
    Private Enum MediaPlaybackStateConstants
        Idle
        Playing
        Paused
    End Enum

    Private Enum FilterMode
        MorphOnly = 0
        MoveOnly = 1
        MorphThenMove = 2
        MoveThenMorph = 3
    End Enum

    Private mMedia As AForge.Video.DirectShow.FileVideoSource
    Private mMediaState As MediaPlaybackStateConstants

    Private interpolationLength As Integer = 3

    Private receivedFramesBuffer As New Queue(Of Bitmap)
    Private interpolatedFramesBuffer As New Queue(Of Bitmap)

    Private playbackThread As Thread
    Private interpolationThread As Thread
    Private interpolationEvent As AutoResetEvent

    Private lastInterpolatedFrame As New Bitmap(1, 1)

    Private morphFilter As New AForge.Imaging.Filters.Morph()
    Private moveTowardsFilter As New AForge.Imaging.Filters.MoveTowards()

    Private f1 As Font = New Font("Tahoma", 32, FontStyle.Bold)
    Private f2 As Font = New Font("Consolas", 16, FontStyle.Bold)
    Private b1 As New SolidBrush(Color.FromArgb(160, Color.White))
    Private b2 As New SolidBrush(Color.FromArgb(128, Color.Black))
    Private b3 As New SolidBrush(Color.FromArgb(160, Color.CadetBlue))

    Private mUseFilters As Boolean = True
    Private mFilterMode As FilterMode = FilterMode.MoveThenMorph

    Private lck As New Object()

    Private Sub FormMain_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        playbackThread.Abort()
        interpolationThread.Abort()

        Application.DoEvents()

        mMedia.SignalToStop()
        mMedia.WaitForStop()
    End Sub

    Private Sub FormMain_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        Select Case e.KeyCode
            Case Keys.Down
                ResetBuffers()
                If interpolationLength > 0 Then interpolationLength -= 1
            Case Keys.Up
                ResetBuffers()
                interpolationLength += 1
            Case Keys.F
                mUseFilters = Not mUseFilters
            Case Keys.Space
                ResetBuffers()
                mMedia.Position = 0
            Case Keys.M
                ResetBuffers()

                mFilterMode += 1
                mFilterMode = mFilterMode Mod (FilterMode.MoveThenMorph + 1)
            Case Keys.Enter
                ResetBuffers()

                interpolationLength = 0
                mUseFilters = False
                mFilterMode = 0
                mMedia.Position = 0
        End Select
    End Sub

    Private Sub ResetBuffers()
        SyncLock lck
            receivedFramesBuffer.Clear()
            interpolatedFramesBuffer.Clear()
        End SyncLock
    End Sub

    Private Sub FormMain_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.SetStyle(ControlStyles.AllPaintingInWmPaint, True)
        Me.SetStyle(ControlStyles.OptimizedDoubleBuffer, True)
        Me.SetStyle(ControlStyles.UserPaint, True)

        AddHandler Me.DoubleClick, Sub()
                                       If Me.WindowState = FormWindowState.Normal Then
                                           Me.WindowState = FormWindowState.Maximized
                                       ElseIf Me.WindowState = FormWindowState.Maximized Then
                                           Me.WindowState = FormWindowState.Normal
                                       End If
                                   End Sub

        LoadMedia(IO.Path.Combine(My.Application.Info.DirectoryPath, "Videos\TOR@BAL- Encarnacion makes nice stretch at first base.avi"))

        playbackThread = New Thread(AddressOf PlaybackSub) With {
            .IsBackground = True
        }
        playbackThread.Start()

        interpolationEvent = New AutoResetEvent(False)

        interpolationThread = New Thread(AddressOf InterpolateFrames) With {
            .IsBackground = True
        }
        interpolationThread.Start()

        mMediaState = MediaPlaybackStateConstants.Playing
        mMedia.Start()
    End Sub

    Private Sub LoadMedia(fileName As String)
        mMedia = New AForge.Video.DirectShow.FileVideoSource(fileName, True)

        AddHandler mMedia.NewFrame, AddressOf RenderFrame
        AddHandler mMedia.PlayingFinished, Sub(sender As Object, e As AForge.Video.ReasonToFinishPlaying)
                                               mMediaState = MediaPlaybackStateConstants.Idle
                                           End Sub

        AddHandler mMedia.VideoSourceError, Sub(sender As Object, e As AForge.Video.VideoSourceErrorEventArgs)
                                                MsgBox("Video Source Error:" + vbCrLf + vbCrLf + e.Description)
                                            End Sub
    End Sub

    Private Sub Pause()
        If mMediaState = MediaPlaybackStateConstants.Playing Then
            mMediaState = MediaPlaybackStateConstants.Paused
        Else
            mMediaState = MediaPlaybackStateConstants.Playing
        End If

        mMedia.Pause()
    End Sub

    Private Sub PlaybackSub()
        Do
            Thread.Sleep(33)

            If receivedFramesBuffer.Count > 3 AndAlso interpolatedFramesBuffer.Count < interpolationLength / 2 Then
                interpolationEvent.Set()
            End If

            If receivedFramesBuffer.Count > 6 AndAlso mMediaState = MediaPlaybackStateConstants.Playing Then
                Pause()
            ElseIf receivedFramesBuffer.Count < 5 AndAlso mMediaState = MediaPlaybackStateConstants.Paused Then
                Pause()
            End If

            Me.Invalidate()
        Loop
    End Sub

    Private Sub RenderFrame(sender As Object, e As AForge.Video.NewFrameEventArgs)
        If interpolationLength = 0 Then
            interpolatedFramesBuffer.Enqueue(ConvertTo24bppRgb(e.Frame))
        Else
            receivedFramesBuffer.Enqueue(ConvertTo24bppRgb(e.Frame))
        End If
    End Sub

    Private Sub InterpolateFrames()
        Do
            interpolationEvent.WaitOne()

            SyncLock lck
                If receivedFramesBuffer.Count = 0 Then Continue Do

                If mUseFilters Then
                    Select Case mFilterMode
                        Case FilterMode.MorphOnly
                            morphFilter.OverlayImage = receivedFramesBuffer.Dequeue()

                            For i As Integer = 0 To interpolationLength - 1
                                morphFilter.SourcePercent = i / interpolationLength

                                interpolatedFramesBuffer.Enqueue(morphFilter.Apply(receivedFramesBuffer(0)))
                            Next

                            morphFilter.OverlayImage.Dispose()
                        Case FilterMode.MoveOnly
                            moveTowardsFilter.OverlayImage = receivedFramesBuffer.Dequeue()

                            For i As Integer = 0 To interpolationLength - 1
                                moveTowardsFilter.StepSize = 256 - 256 * (i / interpolationLength)

                                interpolatedFramesBuffer.Enqueue(moveTowardsFilter.Apply(receivedFramesBuffer(0)))
                            Next

                            moveTowardsFilter.OverlayImage.Dispose()
                        Case FilterMode.MorphThenMove
                            morphFilter.OverlayImage = receivedFramesBuffer.Dequeue()

                            For i As Integer = 0 To interpolationLength - 1
                                morphFilter.SourcePercent = i / interpolationLength
                                moveTowardsFilter.StepSize = 256 - 256 * morphFilter.SourcePercent

                                moveTowardsFilter.OverlayImage = morphFilter.Apply(receivedFramesBuffer(0))
                                interpolatedFramesBuffer.Enqueue(moveTowardsFilter.Apply(receivedFramesBuffer(0)))

                                moveTowardsFilter.OverlayImage.Dispose()
                            Next

                            morphFilter.OverlayImage.Dispose()
                        Case FilterMode.MoveThenMorph
                            moveTowardsFilter.OverlayImage = receivedFramesBuffer.Dequeue()

                            For i As Integer = 0 To interpolationLength - 1
                                morphFilter.SourcePercent = i / interpolationLength
                                moveTowardsFilter.StepSize = 256 - 256 * morphFilter.SourcePercent

                                morphFilter.OverlayImage = moveTowardsFilter.Apply(receivedFramesBuffer(0))
                                interpolatedFramesBuffer.Enqueue(morphFilter.Apply(receivedFramesBuffer(0)))

                                morphFilter.OverlayImage.Dispose()
                            Next

                            moveTowardsFilter.OverlayImage.Dispose()
                    End Select
                Else
                    Using tmpBmp = receivedFramesBuffer.Dequeue()
                        For i As Integer = 0 To interpolationLength - 1
                            interpolatedFramesBuffer.Enqueue(tmpBmp.Clone())
                        Next
                    End Using
                End If
            End SyncLock
        Loop
    End Sub

    Protected Overrides Sub OnPaint(e As PaintEventArgs)
        Dim g As Graphics = e.Graphics

        If interpolatedFramesBuffer.Count > 0 Then
            Dim bmp = interpolatedFramesBuffer.Dequeue()
            g.DrawImage(bmp, Me.DisplayRectangle)
            'g.DrawImageFast(bmp, Point.Empty)

            lastInterpolatedFrame.Dispose()
            lastInterpolatedFrame = bmp
        Else
            'g.DrawImageFast(lastInterpolatedFrame, Point.Empty)
            g.DrawImage(lastInterpolatedFrame, Me.DisplayRectangle)
        End If

        g.FillRectangle(b2, 0, 0, 360, 50 + 22 * (6 + 1))
        g.DrawString(interpolationLength.ToString() + ": " + receivedFramesBuffer.Count.ToString() + "/" + interpolatedFramesBuffer.Count.ToString(), f1, b1, 0, 0)
        g.DrawString(String.Format("[F]ilter:     O{0}", If(mUseFilters, "n", "ff")), f2, b1, 0, 50 + 22 * 0)
        g.DrawString(String.Format("[M]ode:       {0}", mFilterMode.ToString()), f2, If(mUseFilters, b3, b1), 0, 50 + +22 * 1)
        g.DrawString("[Up Arrow]:   Slow Down", f2, b1, 0, 55 + 22 * 2)
        g.DrawString("[Down Arrow]: Speed Up", f2, b1, 0, 55 + 22 * 3)
        g.DrawString("[SPACE]:      Restart", f2, b1, 0, 60 + 22 * 4)
        g.DrawString("[ENTER]:      Reset", f2, b1, 0, 60 + 22 * 5)
    End Sub

    Protected Overrides Sub OnPaintBackground(e As PaintEventArgs)
        'MyBase.OnPaintBackground(e)
    End Sub

    Private Function ConvertTo24bppRgb(bmp As Bitmap)
        If bmp.PixelFormat = Imaging.PixelFormat.Format24bppRgb Then Return bmp.Clone()

        Dim tmpBmp As New Bitmap(bmp.Width, bmp.Height, Imaging.PixelFormat.Format24bppRgb)
        Using g = Graphics.FromImage(tmpBmp)
            g.DrawImageUnscaled(bmp, 0, 0)
        End Using

        bmp.Dispose()

        Return tmpBmp
    End Function
End Class
