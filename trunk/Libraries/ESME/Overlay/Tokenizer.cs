using System;
using System.Collections.Generic;

namespace ESME.Overlay
{
    internal class Tokenizer
    {
        private readonly char[] _sep = {' '};
        private readonly Queue<Token> _tokens = new Queue<Token>();
        private LineReader _lineReader;
        //public char TokenSeparators { get; set; }

        public int Count
        {
            get { return _tokens.Count; }
        }

        public LineReader LineReader
        {
            set
            {
                _lineReader = value;

                string curLine = _lineReader.NextLine();
                while (curLine != null)
                {
                    string[] tokens = curLine.Split(_sep, StringSplitOptions.RemoveEmptyEntries);
                    foreach (var token in tokens)
                    {
                        float f;
                        if (float.TryParse(token, out f))
                            _tokens.Enqueue(new Token
                                                {
                                                    Value = f,
                                                    IsNumeric = true,
                                                    LineNumber = _lineReader.LineNumber,
                                                    LineReader = _lineReader
                                                });
                        else
                        {
                            switch (token.ToLower())
                            {
                                case OverlayKeywords.Red:
                                case OverlayKeywords.Green:
                                case OverlayKeywords.Purple:
                                case OverlayKeywords.Yellow:
                                case OverlayKeywords.White:
                                case OverlayKeywords.Orange:
                                case OverlayKeywords.Blue:
                                case OverlayKeywords.Cyan:
                                    break;
                                default:
                                    _tokens.Enqueue(new Token
                                    {
                                        Value = token,
                                        IsNumeric = false,
                                        LineNumber = _lineReader.LineNumber,
                                        LineReader = _lineReader
                                    });
                                    break;
                            }
                        }
                    }
                    curLine = _lineReader.NextLine();
                }
            }
        }

        public Token NextToken()
        {
            return _tokens.Count == 0 ? null : _tokens.Dequeue();
        }

        public Token Peek()
        {
            return _tokens.Count != 0 ? _tokens.Peek() : null;
        }

        public void DiscardNext()
        {
            if (_tokens.Count > 0)
                _tokens.Dequeue();
        }

        public void DiscardToEndOfLine()
        {
            var curLineNumber = _tokens.Peek().LineNumber;
            while ((_tokens.Count > 0) && (_tokens.Peek().LineNumber == curLineNumber))
                _tokens.Dequeue();
        }
    }

    internal class Token
    {
        public object Value { get; set; }
        public bool IsNumeric { get; set; }
        public int LineNumber { get; set; }
        public LineReader LineReader { get; set; }
    }
}