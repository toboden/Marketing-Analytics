import librosa
import pandas as pd
import numpy as np
import openpyxl
import os
import unicodedata

audio_dir = '/Users/philippgrunenberg/Downloads/download'
audio_features_dir = '/Users/philippgrunenberg/Documents/Marketing Analytics/MA Repo/data/audio_features.xlsx'
ranked_songs_dir = '/Users/philippgrunenberg/Documents/Marketing Analytics/MA Repo/data/Songs mit Ranking_100 Songs.xlsx'


def clean_title(title):
    # Finde das erste Vorkommen von '(' oder '?'
    for char in ['(', '?', '/']:
        if char in title:
            title = title.split(char)[0]  # Nur der Teil vor dem Sonderzeichen
            break
    title = unicodedata.normalize('NFKD', title).encode('ASCII', 'ignore').decode('utf-8')
    return title.strip().lower()

def rename_files(file_list, track_table):
    renamed = 0
    for title, (artist, rating) in track_table.items():
 
        clean_track = clean_title(title)
        clean_artist = clean_title(artist)
        rating_str = str(rating) if pd.notna(rating) else 'x'
        
        # Zuerst nach Titel matchen
        match = next((song for song in file_list if clean_track in clean_title(song)), None)
        if not match:
            # Dann nach Künstler matchen
            match = next((song for song in file_list if clean_artist in clean_title(song)), None)
            if match:
                print(f'Use artist to match: {artist}: {title} -> {match}')
            else:
                print(f'Nicht gefunden: {artist}: {title}')
                continue
            
            
            
        new_name = f'{clean_artist}_{clean_track}_{rating_str}.wav'
        new_path = os.path.join(audio_dir, new_name)
        old_path = os.path.join(audio_dir, match)
        
        try:
          os.rename(old_path, new_path)
          renamed = renamed + 1
          file_list.remove(match)
        except:
          print(f'Error: could not rename {match} to {new_name}')
          
            
    print(f'renamed {renamed} files.')
    print(f'did not rename {len(file_list)} files: ')
    for file in file_list:
      print(file)


def extract_features(file_path):
    # -------------------------------------------------
    # 1) Laden + Basis-Preprocessing
    # -------------------------------------------------
    # Einheitliche Samplerate & Mono
    y, sr = librosa.load(file_path, sr=22050, mono=True)

    # Lautstärke-Normalisierung
    y = librosa.util.normalize(y)

    # Stille entfernen (sehr wichtig für Ratings)
    y, _ = librosa.effects.trim(y, top_db=20)

    # -------------------------------------------------
    # 2) Zeit-Frequenz-Repräsentationen
    # -------------------------------------------------
    # STFT für spektrale Features
    S = np.abs(librosa.stft(y, n_fft=2048, hop_length=512))

    # Mel-Spektrogramm (perzeptiv sinnvoll)
    mel_spec = librosa.feature.melspectrogram(y=y, sr=sr, n_mels=128)
    mel_spec_db = librosa.power_to_db(mel_spec, ref=np.max)

    # -------------------------------------------------
    # 3) Feature-Extraktion
    # -------------------------------------------------

    # --- MFCCs (Klangfarbe / Timbre)
    mfcc = librosa.feature.mfcc(
        S=mel_spec_db,
        sr=sr,
        n_mfcc=13
    )

    # Delta-MFCCs (Dynamik / Veränderung)
    mfcc_delta = librosa.feature.delta(mfcc)

    # --- Spektrale Features (Helligkeit / Schärfe)
    spec_centroid = librosa.feature.spectral_centroid(S=S, sr=sr)
    spec_bandwidth = librosa.feature.spectral_bandwidth(S=S, sr=sr)
    rolloff = librosa.feature.spectral_rolloff(S=S, sr=sr)
    flatness = librosa.feature.spectral_flatness(S=S)

    # --- Rhythmus & Energie
    rms = librosa.feature.rms(S=S)
    zcr = librosa.feature.zero_crossing_rate(y)

    tempo, _ = librosa.beat.beat_track(y=y, sr=sr)

    # --- Harmonie (Tonart / Akkorde)
    chroma = librosa.feature.chroma_stft(S=S, sr=sr)

    # -------------------------------------------------
    # 4) Aggregation (feste Feature-Länge)
    # -------------------------------------------------
    features = {}

    # MFCCs
    for i in range(13):
        features[f'mfcc_{i+1}_mean'] = mfcc[i].mean()
        features[f'mfcc_{i+1}_std'] = mfcc[i].std()
        features[f'mfcc_delta_{i+1}_mean'] = mfcc_delta[i].mean()

    # Spektrale Features
    features['spectral_centroid_mean'] = spec_centroid.mean()
    features['spectral_bandwidth_mean'] = spec_bandwidth.mean()
    features['rolloff_mean'] = rolloff.mean()
    features['flatness_mean'] = flatness.mean()

    # Energie & Rhythmus
    features['rms_mean'] = rms.mean()
    features['rms_std'] = rms.std()
    features['zcr_mean'] = zcr.mean()
    features['tempo'] = tempo

    # Chroma (nur Mittelwerte, keine Std)
    for i in range(12):
        features[f'chroma_{i+1}_mean'] = chroma[i].mean()

    return features



# Excel-Datei einlesen
ranked_songs_xlsx = pd.read_excel(ranked_songs_dir)
# Mapping aus der Excel-Tabelle erstellen
track_table = dict(zip(ranked_songs_xlsx['Track Title'], zip(ranked_songs_xlsx['Artist'], ranked_songs_xlsx['Bewertung'])))
# Dateinamen anpassen
rename_files(os.listdir(audio_dir), track_table)

#features extrahieren
rows = []
for file in os.listdir(audio_dir):
    if file.endswith('.wav'):
        path = os.path.join(audio_dir, file)
        features = extract_features(path)
        features['song'] = file
        rows.append(features)
#write data to excel
feature_df = pd.DataFrame(rows)
feature_df.set_index('song', inplace=True)
feature_df.to_excel(audio_features_dir)
