package main

import (
	"encoding/base64"
	"os"

	"github.com/rs/zerolog"
	"github.com/spf13/pflag"
	yaml "gopkg.in/yaml.v3"
)

type secret struct {
	APIVersion string                 `yaml:"apiVersion"`
	Kind       string                 `yaml:"kind"`
	Data       map[string]string      `yaml:"data"`
	Metadata   map[string]interface{} `yaml:"metadata"`
}

func main() {
	logger := zerolog.New(zerolog.ConsoleWriter{Out: os.Stderr})
	var encode bool
	var decode bool
	var verbose bool
	pflag.BoolVar(&encode, "encode", false, "Encode the given stream")
	pflag.BoolVar(&decode, "decode", false, "Decode the given stream")
	pflag.BoolVar(&verbose, "verbose", false, "Verbose logging")
	pflag.Parse()

	var sec secret

	if verbose {
		logger = logger.Level(zerolog.DebugLevel)
	} else {
		logger = logger.Level(zerolog.InfoLevel)
	}

	if err := yaml.NewDecoder(os.Stdin).Decode(&sec); err != nil {
		logger.Fatal().Err(err).Msg("Failed to decode data.")
	}

	if encode {
		for k, v := range sec.Data {
			encoded := base64.URLEncoding.EncodeToString([]byte(v))
			sec.Data[k] = encoded
		}

	} else {
		for k, v := range sec.Data {
			decoded, err := base64.URLEncoding.DecodeString(v)
			if err != nil {
				logger.Fatal().Err(err).Msgf("Failed to decode value of %s: %s", k, v)
			}
			sec.Data[k] = string(decoded)
		}
	}

	if err := yaml.NewEncoder(os.Stdout).Encode(&sec); err != nil {
		logger.Fatal().Err(err).Msg("Failed to re-encode into YAML.")
	}
}
