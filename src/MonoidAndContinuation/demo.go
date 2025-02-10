
func f(originalWave, filterWave []float64) []float64 {
	extendedWave := make([]float64, len(originalWave)+len(filterWave)-1)
	for i := range originalWave {
		extendedWave[i + len(filterWave) / 2] = originalWave[i]
	}
	for i := 0; i < len(filterWave) / 2; i++ {
		extendedWave[i] = originalWave[0]
		extendedWave[len(extendedWave) - i - 1] = originalWave[len(originalWave) - 1]
	}
	leftPad := make([]float64, padLength[0])
	rightPad := make([]float64, padLength[1] - 1)
	q1Wv = ampArray(amplitude, qubitWv)
	q1Wv = append(append(leftPad, q1Wv...), rightPad...)
	rightPad := make([]float64, len(filterWave)/2)
	extendedWave = append(append(leftPad, extendedWave...), rightPad...)
	rightPad := make([]float64, len(filterWave)/2)
	extendedWave = append(append(leftPad, extendedWave...), rightPad...)
	return extendedWave
}

