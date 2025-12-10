package main

import (
	"context"
	"testing"
)

func TestSayHi(t *testing.T) {
	tests := []struct {
		name    string
		input   Input
		want    string
		wantErr bool
	}{
		{
			name:  "Valid name",
			input: Input{Name: "Gemini"},
			want:  "Hi Gemini",
		},
		{
			name:  "Empty name",
			input: Input{Name: ""},
			want:  "Hi ",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// context and req are not used in the simple logic of SayHi
			ctx := context.Background()

			// Call the function under test
			// We pass nil for *mcp.CallToolRequest as it is unused in the implementation
			_, output, err := SayHi(ctx, nil, tt.input)

			if (err != nil) != tt.wantErr {
				t.Errorf("SayHi() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if output.Greeting != tt.want {
				t.Errorf("SayHi() = %v, want %v", output.Greeting, tt.want)
			}
		})
	}
}
